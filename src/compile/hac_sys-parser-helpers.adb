-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC_Sys.Scanner, HAC_Sys.UErrors;

package body HAC_Sys.Parser.Helpers is

  use Scanner, UErrors;

  procedure Need (
    CD      : in out Compiler_Data;
    S       : KeyWSymbol;
    E       : Compile_Error;
    Forgive : KeyWSymbol := Dummy_Symbol
  )
  is
  begin
    if CD.Sy = S then
      InSymbol (CD);
    else
      Error (CD, E, stop => Forgive = Dummy_Symbol);
      if CD.Sy = Forgive then
        InSymbol (CD);
      end if;
    end if;
  end Need;

  procedure Skip (
    CD   : in out Compiler_Data;
    FSys : Symset;
    N    : Compile_Error;
    hint : String := ""
  )
  is

    function StopMe return Boolean is
    begin
      return False;
    end StopMe;

  begin
    Error (CD, N, hint);
    --
    while not FSys (CD.Sy) loop
      InSymbol (CD);
      if StopMe then
        raise Failure_1_0;
      end if;
    end loop;

    InSymbol (CD);    -- Manuel:  If this InSymbol call is
    --  omitted, the system will get in an
    --  infinite loop on the statement:
    --  put_lin("Typo is on purpose");

    if StopMe then
      raise Failure_1_0;
    end if;
  end Skip;

  procedure Skip (
    CD   : in out Compiler_Data;
    S    : KeyWSymbol;
    N    : Compile_Error;
    hint : String := ""
  )
  is
  begin
    Skip (CD, Singleton (S), N, hint);
  end Skip;

  procedure Test (
    CD            : in out Compiler_Data;
    S1, S2        : Symset;
    N             : Compile_Error;
    stop_on_error : Boolean := False)
  is
    use VStrings_Pkg;
  begin
    if not S1 (CD.Sy) then
      declare
        hint  : VString;
        first : Boolean := True;
      begin
        for s in S1'Range loop
          if S1 (s) then
            if not first then
              hint := hint & ", ";
            end if;
            first := False;
            hint := hint & KeyWSymbol'Image (s);
          end if;
        end loop;
        hint := "Found: " & KeyWSymbol'Image (CD.Sy) & "; expected: " & hint;
        if stop_on_error then
          Error (CD, N, stop => True, hint => Defs.To_String (hint));
        end if;
        Skip (CD, S1 + S2, N, Defs.To_String (hint));
      end;
    end if;
  end Test;

  After_semicolon : constant Symset :=
    (IDent | SUBTYPE_Symbol | TYPE_Symbol | TASK_Symbol => True, others => False) +
    Block_Begin_Symbol;

  Comma_or_colon : constant Symset :=
    Symset'(Comma | Colon => True, others => False);

  procedure Test_Semicolon_in_Declaration (CD : in out Compiler_Data; FSys : Symset) is
  begin
    if CD.Sy = Semicolon then
      InSymbol (CD);
      Ignore_Extra_Semicolons (CD);
    else
      Error (CD, err_semicolon_missing);
      if Comma_or_colon (CD.Sy) then
        InSymbol (CD);
      end if;
    end if;
    Test (CD, After_semicolon, FSys, err_incorrectly_used_symbol);
  end Test_Semicolon_in_Declaration;

  procedure Test_END_Symbol (CD : in out Compiler_Data) is
  begin
    if CD.Sy = END_Symbol then
      InSymbol (CD);
    else
      Skip (CD, Semicolon, err_END_missing);
    end if;
  end Test_END_Symbol;

  procedure Check_Boolean (CD : in out Compiler_Data; T : Typen) is
  begin
    --  NB: T = NOTYP was admitted in SmallAda.
    if T /= Bools then
      Error (CD, err_expecting_a_boolean_expression);
    end if;
  end Check_Boolean;

  procedure Ignore_Extra_Semicolons (CD : in out Compiler_Data) is
  begin
    if CD.Sy = Semicolon then
      Error (CD, err_duplicate_semicolon, stop => True);
      while CD.Sy = Semicolon loop
        InSymbol (CD);
      end loop;
    end if;
  end Ignore_Extra_Semicolons;

  procedure Argument_Type_Not_Supported (CD : in out Compiler_Data) is
  begin
    Error (CD, err_type_conversion_not_supported, "argument type not supported");
  end Argument_Type_Not_Supported;

  function Nice_Image (T : Typen) return String is
  begin
    case T is
      when NOTYP           => return "(undefined type)";
      when Ints            => return "integer type";
      when Chars           => return "Character type";        -- "the" Character type
      when Bools           => return "Boolean type";          -- "the" Boolean type
      when Floats          => return "floating-point type";
      when Arrays          => return "array type";
      when Records         => return "record type";
      when Enums           => return "enumeration type";
      when String_Literals => return "String type";
      when VStrings        => return "VString type";
      when Times           => return "Time type";                   --  "the" Time type
      when Durations       => return "Duration type";               --  "the" Duration type
      when Text_Files      => return "text file type";
    end case;
  end Nice_Image;

  function Enum_Name (CD : Compiler_Data; E_Ref : Index) return String is
  begin
    return To_String (CD.IdTab (E_Ref).Name_with_case);
  end Enum_Name;

  function Nice_Exact_Image (CD : Compiler_Data; xT : Exact_Typ) return String is
  begin
    if xT.TYP = Enums then
      return Nice_Image (xT.TYP) & " (" & Enum_Name (CD, xT.Ref) & ')';
    else
      return Nice_Image (xT.TYP);
    end if;
  end Nice_Exact_Image;

  procedure Type_Mismatch (
    CD               : in out Compiler_Data;
    Err              :        Compile_Error;
    Found, Expected  :        Exact_Typ
  )
  is
  begin
    if Found.TYP /= Expected.TYP then
      Error (CD, Err,
        "found "      & Nice_Exact_Image (CD, Found) &
        ", expected " & Nice_Exact_Image (CD, Expected));
    elsif Found.TYP = Enums then
      Error (CD, Err,
        "found "      & Enum_Name (CD, Found.Ref) &
        ", expected " & Enum_Name (CD, Expected.Ref));
    else
      Error (CD, Err, "not exactly the same " & Nice_Image (Found.TYP));
        --  !! TBD: find the eventual array or record
        --     names using X.Ref, Y.Ref ... if they have names!
        --     (same for Operator_Undefined)
    end if;
  end Type_Mismatch;

  procedure Type_Mismatch (
    CD       : in out Compiler_Data;
    Err      :        Compile_Error;
    Found    :        Exact_Typ;
    Expected :        Typ_Set
  )
  is
    function Types_List (TS : Typ_Set) return String is
      use VStrings_Pkg;
      hint  : VString;
      first : Boolean := True;
    begin
      for s in TS'Range loop
        if TS (s) then
          if not first then
            hint := hint & ", ";
          end if;
          first := False;
          hint := hint & Nice_Image (s);
        end if;
      end loop;
      return Defs.To_String (hint);
    end Types_List;
  begin
    Error (
      CD, Err,
      "found: "      & Nice_Exact_Image (CD, Found) &
      ", expected: " & Types_List (Expected),
      stop => True
    );
  end Type_Mismatch;

  function Op_Hint (OP : KeyWSymbol) return Character is
  --  Displayed as "operator (+) is not defined..."
  begin
    case OP is
      when Plus             => return '+';
      when Minus            => return '-';
      when Times            => return '*';
      when Divide           => return '/';
      when Ampersand_Symbol => return '&';
      when others           => return '?';
    end case;
  end Op_Hint;

  procedure Operator_Undefined (
    CD          : in out Compiler_Data;
    Operator    :        KeyWSymbol;
    Left, Right :        Exact_Typ
  )
  is
  begin
    if Left.TYP /= Right.TYP then
      Error (CD, err_operator_not_defined_for_types,
        Op_Hint (Operator) &
        "left is "    & Nice_Exact_Image (CD, Left) &
        ", right is " & Nice_Exact_Image (CD, Right));
    elsif Left.TYP = Enums then
      Error (CD, err_operator_not_defined_for_types,
        Op_Hint (Operator) &
        "left is "    & Enum_Name (CD, Left.Ref) &
        ", right is " & Enum_Name (CD, Right.Ref));
    else
      Error (CD, err_operator_not_defined_for_types,
        Op_Hint (Operator) &
        "not exactly the same " & Nice_Image (Left.TYP));
        --  !! TBD: find the eventual array or record
        --     names using X.Ref, Y.Ref ... if they have names!
        --     (same for Type_Mismatch)
    end if;
  end Operator_Undefined;

  procedure Forbid_Type_Coercion (
    CD          : in out Compiler_Data;
    Operator    :        KeyWSymbol;
    Left, Right :        Exact_Typ
  )
  is
  begin
    Error (CD,
      err_numeric_type_coercion_operator,
        Op_Hint (Operator) &
        "left is "    & Nice_Exact_Image (CD, Left) &
        ", right is " & Nice_Exact_Image (CD, Right),
      stop => True
    );
  end Forbid_Type_Coercion;

  procedure Forbid_Type_Coercion (
    CD              : in out Compiler_Data;
    Found, Expected :        Exact_Typ
  )
  is
  begin
    Error (CD, err_numeric_type_coercion,
      "found "    & Nice_Exact_Image (CD, Found) &
      ", expected " & Nice_Exact_Image (CD, Expected),
      stop => True);
  end Forbid_Type_Coercion;

  function Singleton (s : KeyWSymbol) return Symset is
    res : Symset := Empty_Symset;
  begin
    res (s) := True;
    return res;
  end Singleton;

  function Is_Char_Array (CD : Compiler_Data; T : Exact_Typ) return Boolean is
  begin
    return T.TYP = Arrays and then CD.Arrays_Table (T.Ref).Element_xTyp.TYP = Chars;
  end Is_Char_Array;

  ------------------------------------------------------------------
  ------------------------------------------------Locate_Identifier-
  function Locate_Identifier (
    CD            : in out Compiler_Data;
    Id            :        Alfa;
    Level         :        HAC_Sys.PCode.Nesting_level;
    No_Id_Fail    :        Boolean := True;
    Stop_on_Error :        Boolean := False
  )
  return Natural
  is
    use HAC_Sys.PCode;
    L : Operand_1_Type;
    J : Integer;
  begin
    L                     := Level;
    CD.IdTab (No_Id).Name := Id;  --  Sentinel
    loop
      J := CD.Blocks_Table (CD.Display (L)).Last_Id_Idx;
      while CD.IdTab (J).Name /= Id loop  --  Scan all Id's on level L.
        J := CD.IdTab (J).Link;
      end loop;
      L := L - 1;  --  Decrease nesting level.
      exit when L < 0 or J /= No_Id;
    end loop;
    if J = No_Id and No_Id_Fail then
      Error (CD, err_undefined_identifier, stop => Stop_on_Error);
    end if;
    --  Name aliasing resolution (brought by a use clause)
    while J /= No_Id and then CD.IdTab (J).Entity = Alias loop
      J := CD.IdTab (J).Adr_or_Sz;  --  E.g. True -> Standard.True
    end loop;
    return J;
  end Locate_Identifier;

  ------------------------------------------------------------------
  ----------------------------------------------Enter_or_find_Float-
  procedure Enter_or_find_Float (
    CD         : in out Compiler_Data;
    X          :        HAC_Float;
    RNum_Index :    out Natural
  )
  is
  begin
    if CD.Float_Constants_Count = Float_Const_Table_Max - 1 then
      Fatal (FLOAT_CONSTANTS);  --  Exception is raised there.
    end if;
    CD.Float_Constants_Table (CD.Float_Constants_Count + 1) := X;  --  We add X's value as an extra item.
    RNum_Index := 1;
    while CD.Float_Constants_Table (RNum_Index) /= X loop  --  Binary equality.
      RNum_Index := RNum_Index + 1;
    end loop;
    if RNum_Index > CD.Float_Constants_Count then  --  X's value was not previously in the table.
      CD.Float_Constants_Count := RNum_Index;
    end if;
  end Enter_or_find_Float;

end HAC_Sys.Parser.Helpers;
