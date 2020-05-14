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

with HAC.Scanner, HAC.UErrors;

package body HAC.Parser.Helpers is

  use HAC.Scanner, HAC.UErrors;

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
      Error (CD, E);
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
    -- omitted, the system will get in an
    -- infinite loop on the statement:
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
    stop_on_error : Boolean:= False)
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
          Error (CD, N, stop_on_error => True, hint => To_String (hint));
        end if;
        Skip (CD, S1 + S2, N, To_String (hint));
      end;
    end if;
  end Test;

  After_semicolon : constant Symset :=
    (IDent | TYPE_Symbol | TASK_Symbol => True, others => False) +
    Block_Begin_Symbol;

  Comma_or_colon : constant Symset :=
    Symset'(Comma | Colon => True, others => False);

  procedure Test_Semicolon (CD : in out Compiler_Data; FSys : Symset) is
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
  end Test_Semicolon;

  procedure Test_END_Symbol (CD : in out Compiler_Data) is
  begin
    if CD.Sy = END_Symbol then
      InSymbol (CD);
    else
      Skip (CD, Semicolon, err_END_missing);
    end if;
  end Test_END_Symbol;

  procedure Check_Boolean (CD : in out Compiler_Data; T: Typen) is
  begin
    --  NB: T = NOTYP was admitted in SmallAda.
    if T /= Bools then
      Error (CD, err_expecting_a_boolean_expression);
    end if;
  end Check_Boolean;

  procedure Ignore_Extra_Semicolons (CD : in out Compiler_Data) is
  begin
    if CD.Sy = Semicolon then
      Error (CD, err_extra_semicolon_ignored);
      while CD.Sy = Semicolon loop
        InSymbol (CD);
      end loop;
    end if;
  end Ignore_Extra_Semicolons;

  procedure Argument_Type_Not_Supported (CD : in out Compiler_Data) is
  begin
    Error (CD, err_type_conversion_not_supported, "argument type not supported");
  end Argument_Type_Not_Supported;

  procedure Forbid_Type_Coercion (CD : in out Compiler_Data; details: String) is
  begin
    Error (CD, err_numeric_type_coercion, details, stop_on_error => True);
  end Forbid_Type_Coercion;

  function Nice_Image (T: Typen) return String is
  begin
    case T is
      when NOTYP           => return "(undefined type)";
      when Ints            => return "integer type";
      when Chars           => return "character type";
      when Bools           => return "boolean type";
      when Floats          => return "floating-point type";
      when Arrays          => return "array type";
      when Records         => return "record type";
      when Enums           => return "enumeration type";
      when String_Literals => return "fixed-size string type";
      when VStrings        => return "variable-size string type";
      when Text_Files      => return "text file type";
    end case;
  end Nice_Image;

  function Enum_Name (CD : Compiler_Data; E_Ref : Index) return String is
  begin
    return To_String (CD.IdTab (E_Ref).Name_with_case);
  end Enum_Name;

  function Nice_Exact_Image (CD : Compiler_Data; xT: Exact_Typ) return String is
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
      return To_String (hint);
    end Types_List;
  begin
    Error (
      CD, Err,
      "found: "      & Nice_Exact_Image (CD, Found) &
      ", expected: " & Types_List (Expected)
    );
  end Type_Mismatch;

  procedure Operator_Undefined (
    CD          : in out Compiler_Data;
    OP          :        KeyWSymbol;
    Left, Right :        Exact_Typ
  )
  is
    function Op_Hint return Character is
    --  Displayed as "operator (+) is not defined..."
    begin
      case OP is
        when Plus             => return '+';
        when Minus            => return '-';
        when Times            => return '*';
        when Ampersand_Symbol => return '&';
        when others           => return '?';
      end case;
    end;
  begin
    if Left.TYP /= Right.TYP then
      Error (CD, err_operator_not_defined_for_types,
        Op_Hint &
        "left is "    & Nice_Exact_Image (CD, Left) &
        ", right is " & Nice_Exact_Image (CD, Right));
    elsif Left.TYP = Enums then
      Error (CD, err_operator_not_defined_for_types,
        Op_Hint &
        "left is "    & Enum_Name (CD, Left.Ref) &
        ", right is " & Enum_Name (CD, Right.Ref));
    else
      Error (CD, err_operator_not_defined_for_types,
        Op_Hint &
        "not exactly the same " & Nice_Image (Left.TYP));
        --  !! TBD: find the eventual array or record
        --     names using X.Ref, Y.Ref ... if they have names!
        --     (same for Type_Mismatch)
    end if;
  end Operator_Undefined;

  function Singleton (s: KeyWSymbol) return Symset is
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
    Level         :        Integer;
    No_Id_Fail    :        Boolean := True;
    stop_on_error :        Boolean := False) return Natural
  is
    L, J : Integer;
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
      Error (CD, err_undefined_identifier, stop_on_error => stop_on_error);
    end if;
    return J;
  end Locate_Identifier;

    ------------------------------------------------------------------
    -------------------------------------------------Get_File_Pointer-
    function Get_File_Pointer (CD : Compiler_Data; Id : Alfa) return Integer is  -- Schoening
    begin   -- locate Id in FileIOTab
      for I in 1 .. CD.File_IO_Table.Kount loop
        if CD.File_IO_Table.Nam (I) (2) = ':' then
          if CD.File_IO_Table.Nam (I) (3 .. CD.File_IO_Table.LNam (I) - 2) =
             Id (1 .. CD.File_IO_Table.LNam (I) - 2)
          then
            return I;
          end if;
        elsif CD.File_IO_Table.Nam (I) = Id (1 .. CD.File_IO_Table.LNam (I)) then
          return I;
        end if;
      end loop;
      return No_File_Index;
    end Get_File_Pointer;

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
      while CD.Float_Constants_Table (RNum_Index) /= X loop
        RNum_Index := RNum_Index + 1;
      end loop;
      if RNum_Index > CD.Float_Constants_Count then  --  X's value was not previously in the table.
        CD.Float_Constants_Count := RNum_Index;
      end if;
    end Enter_or_find_Float;

end HAC.Parser.Helpers;
