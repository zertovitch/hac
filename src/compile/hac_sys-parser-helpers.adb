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

with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

with HAL;

package body HAC_Sys.Parser.Helpers is

  use Scanner, UErrors;

  procedure Need (
    CD      : in out Compiler_Data;
    S       : KeyWSymbol;
    E       : Compile_Error;
    Forgive : KeyWSymbol := Dummy_Symbol
  )
  is
    severity : Error_Severity := medium;
  begin
    if CD.Sy = S then
      InSymbol (CD);
    else
      if Forgive = Dummy_Symbol then
        severity := major;
      end if;
      Error (CD, E, severity => severity);
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
    use HAL;
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
          Error (CD, N, HAL.VStr_Pkg.To_String (hint), major);
        end if;
        Skip (CD, S1 + S2, N, HAL.VStr_Pkg.To_String (hint));
      end;
    end if;
  end Test;

  After_semicolon : constant Symset :=
    Declaration_X_Subprogram_Symbol +
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
      Error (CD, err_duplicate_semicolon, severity => minor);
      while CD.Sy = Semicolon loop
        InSymbol (CD);
      end loop;
    end if;
  end Ignore_Extra_Semicolons;

  procedure Need_Semicolon (CD : in out Compiler_Data) is
  begin
    if CD.Sy = RParent and then CD.prev_sy = RParent then
      Error (CD, err_extra_right_parenthesis, severity => minor);
      while CD.Sy = RParent loop
        InSymbol (CD);
      end loop;
    end if;
    Need (CD, Semicolon, err_semicolon_missing, Forgive => Comma);
    Ignore_Extra_Semicolons (CD);
  end Need_Semicolon;

  procedure Argument_Type_Not_Supported (CD : in out Compiler_Data) is
  begin
    Error (CD, err_type_conversion_not_supported, "argument type not supported");
  end Argument_Type_Not_Supported;

  function Nice_Image (T : Typen) return String is
  begin
    case T is
      when NOTYP               => return "(undefined type)";
      when Ints                => return "integer type";
      when Chars               => return "Character type";        -- "the" Character type
      when Bools               => return "Boolean type";          -- "the" Boolean type
      when Floats              => return "floating-point type";
      when Arrays              => return "array type";
      when Records             => return "record type";
      when Enums               => return "enumeration type";
      when String_Literals     => return "String type [literal]";
      when Strings_as_VStrings => return "String type [pseudo-unconstrained]";
      when VStrings            => return "VString type";
      when Times               => return "Time type";             --  "the" Time type
      when Durations           => return "Duration type";         --  "the" Duration type
      when Text_Files          => return "text file type";
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
      use HAL;
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
      return HAL.VStr_Pkg.To_String (hint);
    end Types_List;
  begin
    Error (
      CD, Err,
      "found: "      & Nice_Exact_Image (CD, Found) &
      ", expected: " & Types_List (Expected),
      major
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
      major
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
      major
    );
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

  procedure Check_any_String_and_promote_to_VString
    (CD : in out Compiler_Data; X : in out Exact_Typ; include_characters : Boolean)
  is
    use Compiler.PCode_Emit, PCode;
    expected_set : Typ_Set :=  VStrings_Set or Str_Lit_Set or Str_as_VStr_Set or Arrays_Set;
  begin
    if include_characters then
      expected_set := expected_set or Chars_Set;
    end if;
    if X.TYP = String_Literals then
      Emit_Std_Funct (CD, SF_Literal_to_VString);
    elsif Is_Char_Array (CD, X) then
      Emit_Std_Funct (CD,
        SF_String_to_VString,
        Operand_1_Type (CD.Arrays_Table (X.Ref).Array_Size)
      );
    elsif X.TYP = VStrings or X.TYP = Strings_as_VStrings then
      null;  --  Already a VString.
    elsif X.TYP = Chars and include_characters then
      Emit_Std_Funct (CD, SF_Char_to_VString);
    else
      Type_Mismatch (
        CD,
        err_parameter_types_do_not_match,
        Found    => X,
        Expected => expected_set
      );
    end if;
    X.TYP := VStrings;
  end Check_any_String_and_promote_to_VString;

  ------------------------------------------------------------------
  ------------------------------------------------Locate_Identifier-
  function Locate_Identifier (
    CD               : in out Compiler_Data;
    Id               : in     Alfa;
    Level            : in     Defs.Nesting_level;
    Fail_when_No_Id  : in     Boolean := True;
    Alias_Resolution : in     Boolean := True;
    Level_0_Match    : in     Boolean := True
  )
  return Natural
  is
    L : Defs.Nesting_level'Base;
    J : Integer;
    ID_Copy : Alfa;
  begin
    L                     := Level;
    CD.IdTab (No_Id).Name := Id;  --  Sentinel
    --  Scan all Id's on level L down to 0:
    loop
      J := CD.Blocks_Table (CD.Display (L)).Last_Id_Idx;
      --  Scan all Id's on level L:
      while CD.IdTab (J).Name /= Id
        or else
            --  Id is matching, but it is a level 0 definition from a previous unit's compilation
            --  which was not yet reactivated.
            --  In that case, we skip the matching Id, except if it is the sentinel.
            (L = 0
              and then Level_0_Match
              and then J /= No_Id                            --  Not the sentinel.
              and then not CD.CUD.level_0_def.Contains (Id)  --  Invisible 0-level definition.
            )
      loop
        J := CD.IdTab (J).Link;
      end loop;
      L := L - 1;  --  Decrease nesting level.
      exit when L < 0 or J /= No_Id;
    end loop;
    if J = No_Id then
      if not Fail_when_No_Id then
        return No_Id;
      end if;
      Error (CD, err_undefined_identifier, To_String (Id), major);  --  Exception raised here.
    end if;
    --  Name aliasing resolution (brought by a use clause
    --  or a simple renames clause).
    while Alias_Resolution and CD.IdTab (J).Entity = Alias loop
      J := CD.IdTab (J).Adr_or_Sz;  --  E.g. True -> Standard.True
    end loop;
    --  Prefixed package resolution.
    if CD.IdTab (J).Entity = Paquetage then
      Skip_Blanks (CD);
      if CD.CUD.c = '.' then  --  We sneak a look at the next symbol.
        ID_Copy := Id;
        --  Here some parsing: entity is a package and there is a dot waiting.
        InSymbol (CD);  --  Consume prefix package identifier.
        Need (CD, Period, err_syntax_error);  --  Accept "Pkg.", reject "Pkg.."
        if CD.Sy = IDent then
          return Locate_Identifier (
            CD,
            To_Alfa (To_String (ID_Copy) & '.' & To_String (CD.Id)),
            Level,
            Fail_when_No_Id
          );
        end if;
        Error (CD, err_identifier_missing, severity => major);
      end if;
    end if;
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
    use type HAC_Float;
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
