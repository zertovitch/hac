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
     HAC_Sys.Errors;

with HAT;

package body HAC_Sys.Parser.Helpers is

  use Scanner, Errors;
  use type HAC_Integer;

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
      Error (CD, E, ": " & KeyWSymbol'Image (S) & " expected", severity);
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
    use HAT;
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
          Error (CD, N, HAT.VStr_Pkg.To_String (hint), major);
        end if;
        Skip (CD, S1 + S2, N, HAT.VStr_Pkg.To_String (hint));
      end;
    end if;
  end Test;

  After_Semicolon_after_Declaration : constant Symset :=
    Declaration_X_Subprogram_Symbol +
    Block_Begin_Symbol +
    END_Symbol +
    PRIVATE_Symbol;

  Comma_or_colon : constant Symset :=
    Symset'(Comma | Colon => True, others => False);

  procedure Need_Semicolon_after_Declaration (CD : in out Compiler_Data; FSys : Symset) is
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
    Test (CD, After_Semicolon_after_Declaration, FSys, err_incorrectly_used_symbol);
  end Need_Semicolon_after_Declaration;

  procedure Need_END_Symbol (CD : in out Compiler_Data) is
  begin
    if CD.Sy = END_Symbol then
      InSymbol (CD);
    else
      Skip (CD, Semicolon, err_END_missing);
    end if;
  end Need_END_Symbol;

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
    (A2S (CD.IdTab (E_Ref).name_with_case));

  function Nice_Exact_Image (CD : Compiler_Data; xT : Exact_Typ'Class) return String is
    (Nice_Image (xT.TYP) &
      (if xT.TYP = Enums then " (" & Enum_Name (CD, xT.Ref) & ')' else ""));

  procedure Type_Mismatch (
    CD               : in out Compiler_Data;
    Err              :        Compile_Error;
    Found, Expected  :        Exact_Typ'Class
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
    Found    :        Exact_Subtyp;
    Expected :        Typ_Set
  )
  is
    function Types_List (TS : Typ_Set) return String is
      use HAT;
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
      return HAT.VStr_Pkg.To_String (hint);
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
    Left, Right :        Exact_Subtyp
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
    Left, Right :        Exact_Subtyp
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
    Found, Expected :        Exact_Subtyp
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

  function Is_Char_Array (CD : Compiler_Data; T : Exact_Subtyp) return Boolean is
  begin
    return T.TYP = Arrays and then CD.Arrays_Table (T.Ref).Element_xTyp.TYP = Chars;
  end Is_Char_Array;

  procedure Check_any_String_and_promote_to_VString
    (CD : in out Compiler_Data; X : in out Exact_Subtyp; include_characters : Boolean)
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
    X.Construct_Root (VStrings);
  end Check_any_String_and_promote_to_VString;

  ------------------------------------------------------------------
  ------------------------------------------------Locate_Identifier-
  function Locate_Identifier (
    CD               : in out Compiler_Data;
    Id               : in     Alfa;
    Level            : in     Defs.Nesting_level;
    Fail_when_No_Id  : in     Boolean := True;
    Alias_Resolution : in     Boolean := True;
    Level_0_Filter   : in     Boolean := True;
    Public_Filter    : in     Index   := Index'Last
  )
  return Natural
  is
    L : Defs.Nesting_level'Base;
    J : Integer := 0;
    ID_Copy : Alfa;
    is_name_matched : Boolean;
    dot_pos : Integer;
    l0_def : Id_Maps.Cursor;
    use HAT, Id_Maps;
  begin
    L := Level;
    --  Scan all Id's on level L down to 0:
    loop
      if L = 0 and then Level_0_Filter then
        l0_def := CD.CUD.level_0_def.Find (Id);
        if l0_def /= No_Element then
          --  In this case there is no point
          --  doing a tedious linear search :-) .
          J := Element (l0_def);
          exit;
        end if;
      end if;
      J := CD.Blocks_Table (CD.Display (L)).Last_Id_Idx;
      --  Scan all Id's on level L:
      loop
        exit when J = No_Id;  --  Beginning of ID table reached.
        if CD.IdTab (J).entity /= Paquetage_Body then
          --  ^ A package body is invisible as a declaration.
          dot_pos := Length (CD.pkg_prefix);
          if dot_pos = 0 then
            is_name_matched := CD.IdTab (J).name = Id;
          else
            --  We are within a package declaration.
            --  Things are a bit more complicated: the package's items
            --  are visible to the package itself. So we simulate a hidden USE.
            --
            --  Say we are within `Pkg.Child_1.Subpackage_2` declaration.
            --  For entry `Pkg.Child_1.Subpackage_2.Item` in the identifier
            --  table, `Item` is visible, as well as `Subpackage_2[.Item]`
            --  `Child_1[.Subpackage_2[.Item]]`.
            --  NB : the stuff with [] is resolved at the end of Locate_Identifier.
            loop
              is_name_matched :=
                CD.IdTab (J).name =
                  Slice (CD.pkg_prefix, 1, dot_pos) & Id;
              exit when is_name_matched;
              exit when dot_pos = 0;
              loop
                dot_pos := dot_pos - 1;
                exit when dot_pos = 0;
                exit when Element (CD.pkg_prefix, dot_pos) = '.';
              end loop;
            end loop;
          end if;
          if is_name_matched then
            --  Reasons to consider the matched identifier:
            --    * Not library-level: we have a local subprogram
            --        identifier (eventually wrapped in a local package):
            exit when L > 0;
            --    * Filter for library-level definition is disabled:
            exit when not Level_0_Filter;
            --    * Activated library-level definition:
            exit when CD.CUD.level_0_def.Contains (CD.IdTab (J).name);
          end if;
        end if;
        J := CD.IdTab (J).link;  --  Skip this identifier.
      end loop;
      L := L - 1;  --  Decrease nesting level.
      exit when L < 0 or J /= No_Id;
    end loop;
    if J = No_Id then
      if not Fail_when_No_Id then
        return No_Id;
      end if;
      Error (CD, err_undefined_identifier, A2S (Id), major);  --  Exception raised here.
    end if;
    --
    --  From this point, the identifier ID is matched with
    --  element J in the identifier table.
    --

    --  Name aliasing resolution (brought by a use clause
    --  or a simple renames clause):
    while Alias_Resolution and then CD.IdTab (J).entity = Alias loop
      J := CD.IdTab (J).adr_or_sz;  --  E.g. True -> Standard.True
    end loop;

    if J > Public_Filter then
      Error (CD, err_non_public_entity, A2S (Id), major);
    end if;

    --  Prefixed package resolution: `Pkg.Item`, `Pkg.Child_1.Item`, ...
    if CD.IdTab (J).entity = Paquetage then
      Skip_Blanks (CD);
      if CD.CUD.c = '.' then  --  We sneak a look at the next symbol.
        ID_Copy := Id;
        --  Here some parsing: entity is a package and there is a dot waiting.
        InSymbol (CD);  --  Consume prefix package identifier.
        Need (CD, Period, err_syntax_error);  --  Accept "Pkg.", reject "Pkg.."
        if CD.Sy = IDent then
          return Locate_Identifier (
            CD,
            ID_Copy & '.' & CD.Id,
            Level,
            Fail_when_No_Id,
            Alias_Resolution,
            Level_0_Filter,
            CD.Packages_Table (CD.IdTab (J).block_or_pkg_ref).last_public_declaration
          );
        end if;
        Error (CD, err_identifier_missing, severity => major);
      end if;
    end if;
    return J;
  end Locate_Identifier;

  procedure Check_Duplicate_Specification
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     id_current :        Alfa)
  is
  begin
    if old_id_idx = No_Id then
      return;  --  First occurrence of the specification.
    end if;
    Error (CD, err_duplicate_identifier, "specification of " & A2S (id_current), major);
  end Check_Duplicate_Specification;

  procedure Check_Subprogram_Spec_Body_Consistency
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     new_id_idx :        Natural;
     id_current :        Alfa)
  is
    sub_sub_last_param_idx, forward_last_param_idx,
    sub_sub_params, forward_params,
    f, s : Natural;
    use type Alfa;
  begin
    if old_id_idx = No_Id then
      return;
    end if;
    CD.IdTab (old_id_idx).decl_kind := spec_resolved;
    --  The following is only for making the compiler dump
    --  easier to understand:
    CD.Blocks_Table (CD.IdTab (old_id_idx).block_or_pkg_ref).Id :=
      S2A ("Unused (was from a subprogram spec)");
    --  Check that the formal parameter list is identical:
    sub_sub_last_param_idx :=
      CD.Blocks_Table (CD.IdTab (new_id_idx).block_or_pkg_ref).Last_Param_Id_Idx;
    forward_last_param_idx :=
      CD.Blocks_Table (CD.IdTab (old_id_idx).block_or_pkg_ref).Last_Param_Id_Idx;
    sub_sub_params := sub_sub_last_param_idx - new_id_idx;
    forward_params := forward_last_param_idx - old_id_idx;
    if sub_sub_params > forward_params then
      Error (CD, err_number_of_parameters_do_not_match,
             ": specification of " & A2S (id_current) & " has less parameters",
             major);
    elsif sub_sub_params < forward_params then
      Error (CD, err_number_of_parameters_do_not_match,
             ": specification of " & A2S (id_current) & " has more parameters",
             major);
    end if;
    --  Check the formal parameter list:
    f := old_id_idx + 1;
    s := new_id_idx + 1;
    for count in 1 .. sub_sub_params loop
      if CD.IdTab (s).name /= CD.IdTab (f).name then
        Error (CD, err_spec_body_mismatch,
               "parameter #" & Integer'Image (count) & " has a different name",
             major);
        exit;
      end if;
      if CD.IdTab (s).xtyp /= CD.IdTab (f).xtyp then
        Error (CD, err_spec_body_mismatch,
               "parameter #" & Integer'Image (count) & " has a different type",
             major);
        exit;
      end if;
      f := f + 1;
      s := s + 1;
    end loop;
    if CD.IdTab (new_id_idx).entity = Funktion
      and then CD.IdTab (new_id_idx).xtyp /= CD.IdTab (old_id_idx).xtyp
    then
      Error (CD, err_spec_body_mismatch, "result type is different",
             major);
    end if;
  end Check_Subprogram_Spec_Body_Consistency;

  procedure Link_Forward_Declaration
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     new_id_idx :        Natural)
  is
  begin
    --  Clone key information at the new id's index (the body)
    --  onto the data at old id's index (the specification):
    --    * Subprogram's machine code address
    --    * The block_ref (hence, the correct VSize
    --        is used for reserving the stack)
    --
    CD.IdTab (old_id_idx).adr_or_sz        := CD.IdTab (new_id_idx).adr_or_sz;
    CD.IdTab (old_id_idx).block_or_pkg_ref := CD.IdTab (new_id_idx).block_or_pkg_ref;
  end Link_Forward_Declaration;

  procedure Check_Incomplete_Definitions
    (CD    : in out Co_Defs.Compiler_Data;
     Level :        Defs.Nesting_level)
  is
    i : Integer := CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx;
  begin
    --  Follow the chain of identifiers for given Level:
    while i /= Co_Defs.No_Id loop
      if CD.IdTab (i).decl_kind = spec_unresolved
         and then CD.IdTab (i).entity /= aEntry
      then
        Error (CD, err_incomplete_declaration, A2S (CD.IdTab (i).name_with_case));
      end if;
      i := CD.IdTab (i).link;
    end loop;
  end Check_Incomplete_Definitions;

end HAC_Sys.Parser.Helpers;
