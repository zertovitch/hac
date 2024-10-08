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

  procedure Need
    (CD      : in out Compiler_Data;
     S       :        Symbol;
     E       :        Compile_Diagnostic;
     Forgive :        Symbol := Dummy_Symbol)
  is
    severity : Error_Severity := medium;
  begin
    if CD.Sy = S then
      In_Symbol (CD);
    else
      if Forgive = Dummy_Symbol then
        severity := major;
      end if;
      Error (CD, E, ": " & Symbol'Image (S) & " expected", severity => severity);
      if CD.Sy = Forgive then
        In_Symbol (CD);
      end if;
    end if;
  end Need;

  procedure Error_then_Skip
    (CD   : in out Compiler_Data;
     FSys :        Symset;
     N    :        Compile_Diagnostic;
     hint :        String := "")
  is

    function StopMe return Boolean is
    begin
      return False;
    end StopMe;

  begin
    Error (CD, N, hint);
    --
    while not FSys (CD.Sy) loop
      In_Symbol (CD);
      if StopMe then
        raise Failure_1_0;
      end if;
    end loop;

    In_Symbol (CD);
    --  Manuel:  If this In_Symbol call is
    --  omitted, the system will get in an
    --  infinite loop on the statement:
    --  put_lin("Typo is on purpose");

    if StopMe then
      raise Failure_1_0;
    end if;
  end Error_then_Skip;

  procedure Error_then_Skip
    (CD   : in out Compiler_Data;
     S    :        Symbol;
     N    :        Compile_Diagnostic;
     hint :        String := "")
  is
  begin
    Error_then_Skip (CD, Singleton (S), N, hint);
  end Error_then_Skip;

  procedure Test
    (CD            : in out Compiler_Data;
     S1, S2        :        Symset;
     N             :        Compile_Diagnostic;
     stop_on_error :        Boolean := False)
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
            hint := hint & Symbol'Image (s);
          end if;
        end loop;
        hint := "Found: " & Symbol'Image (CD.Sy) & "; expected: " & hint;
        if stop_on_error then
          Error (CD, N, HAT.VStr_Pkg.To_String (hint), severity => major);
        end if;
        Error_then_Skip (CD, S1 + S2, N, HAT.VStr_Pkg.To_String (hint));
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
      In_Symbol (CD);
      Ignore_Extra_Semicolons (CD);
    else
      Error (CD, err_semicolon_missing);
      if Comma_or_colon (CD.Sy) then
        In_Symbol (CD);
      end if;
    end if;
    Test (CD, After_Semicolon_after_Declaration, FSys, err_incorrectly_used_symbol);
  end Need_Semicolon_after_Declaration;

  procedure Need_END_Symbol (CD : in out Compiler_Data) is
  begin
    if CD.Sy = END_Symbol then
      In_Symbol (CD);
    else
      Error_then_Skip (CD, Semicolon, err_END_missing);
    end if;
  end Need_END_Symbol;

  procedure Check_Boolean (CD : in out Compiler_Data; T : Typen) is
  begin
    if T /= Bools then
      Error (CD, err_expecting_a_boolean_expression);
    end if;
  end Check_Boolean;

  procedure Check_Integer (CD : in out Compiler_Data; T : Typen) is
  begin
    if T /= Ints then
      Error (CD, err_parameter_must_be_Integer);
    end if;
  end Check_Integer;

  procedure Ignore_Extra_Semicolons (CD : in out Compiler_Data) is
  begin
    if CD.Sy = Semicolon then
      Error (CD, err_duplicate_semicolon, severity => minor);
      while CD.Sy = Semicolon loop
        In_Symbol (CD);
      end loop;
    end if;
  end Ignore_Extra_Semicolons;

  procedure Need_Semicolon (CD : in out Compiler_Data) is
  begin
    if CD.Sy = RParent and then CD.prev_sy = RParent then
      Error (CD, err_extra_right_parenthesis, severity => minor);
      while CD.Sy = RParent loop
        In_Symbol (CD);
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
      when Ints                => return "an integer type";
      when Chars               => return "Character";        -- "the" Character type
      when Bools               => return "Boolean";          -- "the" Boolean type
      when Floats              => return "a floating-point type";
      when Arrays              => return "an array type";
      when Records             => return "a record type";
      when Enums               => return "an enumeration type";
      when String_Literals     => return "String [literal]";
      when Strings_as_VStrings => return "String [pseudo-unconstrained]";
      when VStrings            => return "VString";
      when Times               => return "Time";             --  "the" Time type
      when Durations           => return "Duration";         --  "the" Duration type
      when Text_Files          => return "File_Type";
    end case;
  end Nice_Image;

  function Enum_Name (CD : Compiler_Data; E_Ref : Index) return String is
    (A2S (CD.IdTab (E_Ref).name_with_case));

  function Nice_Exact_Image (CD : Compiler_Data; xT : Exact_Typ'Class) return String is
    (Nice_Image (xT.TYP) &
      (if xT.TYP = Enums then " (" & Enum_Name (CD, xT.Ref) & ')' else ""));

  procedure Type_Mismatch
    (CD               : in out Compiler_Data;
     Err              :        Compile_Diagnostic;
     Found, Expected  :        Exact_Typ'Class)
  is
  begin
    if Found.TYP /= Expected.TYP then
      Error (CD,
        Err,
        "found "      & Nice_Exact_Image (CD, Found) &
        ", expected " & Nice_Exact_Image (CD, Expected),
        severity => major);
    elsif Found.TYP = Enums then
      Error (CD,
        Err,
        "found """        & Enum_Name (CD, Found.Ref) &
        """, expected """ & Enum_Name (CD, Expected.Ref) & '"',
        severity => major);
    else
      Error (CD,
        Err,
        "not exactly the same " & Nice_Image (Found.TYP),
        severity => major);
      --  Possible improvement: find the possible array or record
      --  names using X.Ref, Y.Ref ... if they have names!
      --  (same for Issue_Undefined_Operator_Error)
    end if;
  end Type_Mismatch;

  procedure Type_Mismatch
    (CD       : in out Compiler_Data;
     Err      :        Compile_Diagnostic;
     Found    :        Exact_Subtyp;
     Expected :        Typ_Set)
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
    Error
      (CD, Err,
       "found: "      & Nice_Exact_Image (CD, Found) &
       ", expected: " & Types_List (Expected),
       severity => major);
  end Type_Mismatch;

  function Op_Hint (OP : Symbol) return String is
  --  Displayed as "operator (+) is not defined..."
  begin
    case OP is
      when Plus             => return "+";
      when Minus            => return "-";
      when Times            => return "*";
      when Divide           => return "/";
      when Power            => return "**";
      when Ampersand_Symbol => return "&";
      when EQL              => return "=";
      when NEQ              => return "/=";
      when GTR              => return ">";
      when GEQ              => return ">=";
      when LSS              => return "<";
      when LEQ              => return "<=";
      when others           => return "?";
    end case;
  end Op_Hint;

  procedure Issue_Undefined_Operator_Error
    (CD       : in out Compiler_Data;
     Operator :        Symbol;
     Right    :        Exact_Subtyp)
  is
  begin
    if Right.TYP = Enums then
      Error
        (CD, err_operator_not_defined_for_types,
         Op_Hint (Operator),
         Enum_Name (CD, Right.Ref));
    else
      Error
        (CD, err_operator_not_defined_for_types,
         Op_Hint (Operator),
         Nice_Image (Right.TYP));
    end if;
  end Issue_Undefined_Operator_Error;

  procedure Issue_Undefined_Operator_Error
    (CD          : in out Compiler_Data;
     Operator    :        Symbol;
     Left, Right :        Exact_Subtyp)
  is
  begin
    if Left.TYP /= Right.TYP then
      Error
        (CD, err_operator_not_defined_for_types,
         Op_Hint (Operator),
         "left is "    & Nice_Exact_Image (CD, Left) &
         ", right is " & Nice_Exact_Image (CD, Right));
    elsif Left.TYP = Enums then
      if Left.Ref = Right.Ref then
        Error
          (CD, err_operator_not_defined_for_types,
           Op_Hint (Operator),
           Enum_Name (CD, Left.Ref));
      else
        Error
          (CD, err_operator_not_defined_for_types,
           Op_Hint (Operator),
           "left is "    & Enum_Name (CD, Left.Ref) &
           ", right is " & Enum_Name (CD, Right.Ref));
      end if;
    else
      Error
        (CD, err_operator_not_defined_for_types,
         Op_Hint (Operator),
         Nice_Image (Left.TYP));
         --  Possible improvement: find the possible array or record
         --  names using X.Ref, Y.Ref ... if they have names!
         --  (same for Type_Mismatch)
    end if;
  end Issue_Undefined_Operator_Error;

  procedure Forbid_Type_Coercion
    (CD          : in out Compiler_Data;
     Operator    :        Symbol;
     Left, Right :        Exact_Subtyp)
  is
  begin
    Error
      (CD,
       err_numeric_type_coercion_operator,
       Op_Hint (Operator),
       "left is "    & Nice_Exact_Image (CD, Left) &
       ", right is " & Nice_Exact_Image (CD, Right),
       major);
  end Forbid_Type_Coercion;

  procedure Forbid_Type_Coercion
    (CD              : in out Compiler_Data;
     Found, Expected :        Exact_Subtyp)
  is
  begin
    Error
      (CD, err_numeric_type_coercion,
       "found "    & Nice_Exact_Image (CD, Found) &
       ", expected " & Nice_Exact_Image (CD, Expected),
       severity => major);
  end Forbid_Type_Coercion;

  function Singleton (s : Symbol) return Symset is
    res : Symset := empty_symset;
  begin
    res (s) := True;
    return res;
  end Singleton;

  function Singleton (t : Typen) return Typ_Set is
    res : Typ_Set := empty_typ_set;
  begin
    res (t) := True;
    return res;
  end Singleton;

  function Is_Char_Array (CD : Compiler_Data; T : Exact_Subtyp) return Boolean is
  begin
    return
      T.TYP = Arrays
        and then T.Ref in Arrays_Table_Type'Range
                 --  ^ Filter out invalid reference due to an error
                 --    that has occurred previously in the parsing.
        and then CD.Arrays_Table (T.Ref).Element_xTyp.TYP = Chars;
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
    elsif Internally_VString_Set (X.TYP) then
      null;  --  Already a VString.
    elsif include_characters and then X.TYP = Chars then
      Emit_Std_Funct (CD, SF_Char_to_VString);
    else
      Type_Mismatch
        (CD,
         err_parameter_types_do_not_match,
         Found    => X,
         Expected => expected_set);
    end if;
    X.Construct_Root (VStrings);
  end Check_any_String_and_promote_to_VString;

  ------------------------------------------------------------------
  ------------------------------------------------Locate_Identifier-

  function Locate_Identifier_Internal
    (CD               : in out Compiler_Data;
     Id               : in     Alfa;
     using_parsed_Id  : in     Boolean;
     Prefix_Id        : in     Alfa;
     Level            : in     Defs.Nesting_Level;
     Fail_when_No_Id  : in     Boolean;
     Alias_Resolution : in     Boolean;
     Level_0_Filter   : in     Boolean;
     Public_Filter    : in     Index)
  return Natural
  is
    L : Defs.Nesting_Level'Base;
    J : Integer := No_Id;
    ID_Copy : Alfa;
    is_name_matched : Boolean;
    dot_pos : Integer;
    l0_def : Id_Maps.Cursor;
    use HAT, Id_Maps;
    trace_search : constant Boolean := False;
  begin
    L := Level;
    if trace_search then
      Put_Line
        (+"  Start Id search, at level" & L'Image & " for Id = """ & Id & '"');
    end if;

    --  Scan all Id's from level L down to level 0:
  Scan :
    loop

      if L = 0 and then Level_0_Filter then
        l0_def := CD.CUD.level_0_def.Find (Id);
        if l0_def /= No_Element then
          --  Global definition found, within a library package, or in the
          --  WITH context clauses.
          --  In this case there is no point doing a tedious linear search :-) .
          J := Element (l0_def);
          if trace_search then
            Put_Line
              (+"    Shortcut! Found global (level 0) definition with value # " & J);
          end if;
          exit Scan;
        end if;
      end if;
      J := CD.Blocks_Table (CD.Display (L)).Last_Id_Idx;
      if trace_search then
        Put_Line
          (+"    Reset index for level" & L'Image &
            " with value # " & J);
      end if;

    Scan_level_L :
      loop
        exit Scan_level_L when J = No_Id;  --  Beginning of ID table reached.

        if CD.IdTab (J).entity /= paquetage_body then
          --  ^ A package body is invisible as a declaration.
          if CD.IdTab (J).entity = paquetage
            and then Length (Prefix_Id) > 0
            and then Prefix_Id = CD.IdTab (J).name
          then
            --  We have reached the defining package.
            --  Example: we are looking for HAT.PUT, but we just hit a
            --  local HAT package's specification. Logically that local HAT
            --  doesn't contain a PUT item, otherwise we would have found
            --  it already.
            --  Other packages with the same name at upper levels are hidden.
            J := No_Id;
            exit Scan;
          end if;
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
            --  NB : the stuff with [] is resolved at the end of Locate_Identifier_Internal.
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

          if trace_search then
            Put_Line
              (+"      Id search level" & L'Image &
               "; candidate # " & J & " named " & CD.IdTab (J).name &
               "; matched : " & is_name_matched'Image);
          end if;

          if is_name_matched then
            --  Reasons to consider the matched identifier:
            --    * Not library-level: we have a local subprogram
            --        identifier (possibly wrapped in a local package):
            exit Scan_level_L when L > 0;
            --    * Filter for library-level definition is disabled:
            exit Scan_level_L when not Level_0_Filter;
            --    * Activated library-level definition:
            exit Scan_level_L when CD.CUD.level_0_def.Contains (CD.IdTab (J).name);
          end if;
        end if;

        if trace_search then
          Put (+"      Chained list: identifier index goes from J = " & J);
        end if;
        J := CD.IdTab (J).link;  --  Skip this identifier.
        if trace_search then
          Put_Line (+" to J = " & J);
        end if;

      end loop Scan_level_L;

      L := L - 1;  --  Decrease nesting level.
      if trace_search then
        Put_Line (+"    Level decreases to L =" & L'Image);
      end if;
      exit Scan when L < 0 or J /= No_Id;
    end loop Scan;

    if J = No_Id then
      if not Fail_when_No_Id then
        return No_Id;
      end if;
      --  Issue an error, severity: major (an exception is raised).
      ID_Copy := CD.Id_with_case;
      In_Symbol (CD);
      if CD.Sy = Finger then
        Error
          (CD,
           err_not_yet_implemented,
           "positional association",
           severity => major);
      else
        Error (CD, err_undefined_identifier, A2S (ID_Copy), severity => major);
      end if;
    end if;
    --
    --  From this point, the identifier ID is matched with
    --  element J in the identifier table.
    --

    --  Name aliasing resolution (brought by a use clause
    --  or a simple renames clause):
    while Alias_Resolution and then CD.IdTab (J).entity = alias loop
      J := Integer (CD.IdTab (J).adr_or_sz);  --  E.g. True -> Standard.True
    end loop;

    if J > Public_Filter then
      Error (CD, err_non_public_entity, A2S (Id), severity => major);
    end if;

    --  Prefixed package resolution: `Pkg.Item`, `Pkg.Child_1.Item`, ...
    if using_parsed_Id and then CD.IdTab (J).entity = paquetage then
      Skip_Blanks (CD);
      if CD.CUD.c = '.' then  --  We sneak a look at the next symbol.
        ID_Copy := Id;
        CD.target.Mark_Reference (J);
        CD.IdTab (J).is_referenced := True;
        --  Here some parsing: entity is a package and there is a dot waiting.
        In_Symbol (CD);  --  Consume prefix package identifier.
        Need (CD, Period, err_general_error);  --  Accept "Pkg.", reject "Pkg.."
        if CD.Sy = IDent then
          return Locate_Identifier_Internal
            (CD,
             ID_Copy & '.' & CD.Id,
             True,
             ID_Copy,
             Level,
             Fail_when_No_Id,
             Alias_Resolution,
             Level_0_Filter,
             CD.Packages_Table (CD.IdTab (J).block_or_pkg_ref).last_public_declaration);
        end if;
        Error (CD, err_identifier_missing, severity => major);
      end if;
    end if;

    if J /= No_Id then
      CD.target.Mark_Reference (J);
      CD.IdTab (J).is_referenced := True;
    end if;

    if trace_search then
      Put_Line (+"  Found identifier # " & J);
    end if;
    return J;
  end Locate_Identifier_Internal;

  function Locate_Identifier
    (CD               : in out Compiler_Data;
     Id               : in     Alfa;
     Level            : in     Defs.Nesting_Level;
     Fail_when_No_Id  : in     Boolean := True;
     Alias_Resolution : in     Boolean := True;
     Level_0_Filter   : in     Boolean := True;
     Public_Filter    : in     Index   := Index'Last)
  return Natural
  is
  begin
    return
      Locate_Identifier_Internal
        (CD               => CD,
         Id               => Id,
         using_parsed_Id  => False,       --  Extra parameter
         Prefix_Id        => Empty_Alfa,  --  Extra parameter
         Level            => Level,
         Fail_when_No_Id  => Fail_when_No_Id,
         Alias_Resolution => Alias_Resolution,
         Level_0_Filter   => Level_0_Filter,
         Public_Filter    => Public_Filter);
  end Locate_Identifier;

  function Locate_CD_Id
    (CD               : in out Compiler_Data;
     Level            : in     Defs.Nesting_Level;
     Fail_when_No_Id  : in     Boolean := True;
     Alias_Resolution : in     Boolean := True;
     Level_0_Filter   : in     Boolean := True;
     Public_Filter    : in     Index   := Index'Last)
  return Natural
  is
  begin
    if CD.Id_location = No_Id_Cache then
      CD.Id_location :=
        --  Note that the following call might consume several
        --  identifiers in the case of, e.g., Pkg_1.Sub_Pkg_2.Item.
        Locate_Identifier_Internal
          (CD               => CD,
           Id               => CD.Id,       --  Here we use the Id just parsed.
           using_parsed_Id  => True,        --  Extra parameter
           Prefix_Id        => Empty_Alfa,  --  Extra parameter
           Level            => Level,
           Fail_when_No_Id  => Fail_when_No_Id,
           Alias_Resolution => Alias_Resolution,
           Level_0_Filter   => Level_0_Filter,
           Public_Filter    => Public_Filter);
    else
      --  In some cases, the same identifier token, at the same location in
      --  source text, is parsed twice or more times.
      --  Example:
      --    "for i in some_array'Range loop": some_array is checked for
      --     begin a subtype (in Static_Subtype_Indication) then for being
      --     a variable (in Simple_Expression). Finally, a pair of values
      --     are detected via the 'Range attribute.
      null;
    end if;
    return CD.Id_location;
  end Locate_CD_Id;

  procedure Check_Duplicate_Specification
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     id_current :        Alfa)
  is
  begin
    if old_id_idx = No_Id then
      return;  --  First occurrence of the specification.
    end if;
    Error
      (CD, err_duplicate_identifier, "specification of " & A2S (id_current), severity => major);
  end Check_Duplicate_Specification;

  procedure Check_Subprogram_Spec_Body_Consistency
    (CD         : in out Compiler_Data;
     old_id_idx :        Natural;
     new_id_idx :        Natural;
     id_current :        Alfa)
  is
    sub_sub_last_param_idx, forward_last_param_idx,
    sub_sub_params, forward_params : Natural;
    use type Alfa;

    procedure Check_Formal_Parameter_List is
      procedure Check_One_to_One
        (prefix       : String;
         is_identical : Boolean;
         topic        : String)
      is
      begin
        if not is_identical then
          Error
            (CD, err_spec_body_mismatch,
             prefix & " has a different " & topic,
             severity => major);  --  This raises an exception.
        end if;
      end Check_One_to_One;

      procedure Check_Parameter_One_to_One
        (count        : Positive;
         is_identical : Boolean;
         topic        : String)
      is
      begin
        Check_One_to_One
          ("parameter #" & count'Image, is_identical, topic);
      end Check_Parameter_One_to_One;

      version_1, version_2 : Natural;
    begin
      version_1 := old_id_idx + 1;
      version_2 := new_id_idx + 1;

      for count in 1 .. sub_sub_params loop
        Check_Parameter_One_to_One
          (count,
           CD.IdTab (version_1).name = CD.IdTab (version_2).name,
           "name");
        Check_Parameter_One_to_One
          (count,
           CD.IdTab (version_1).xtyp = CD.IdTab (version_2).xtyp,
           "type");
        Check_Parameter_One_to_One
          (count,
           CD.IdTab (version_1).decl_kind = CD.IdTab (version_2).decl_kind,
           "mode");

        version_1 := version_1 + 1;
        version_2 := version_2 + 1;
      end loop;

      if CD.IdTab (new_id_idx).entity = funktion then
        Check_One_to_One
          ("result of function",
           CD.IdTab (new_id_idx).xtyp = CD.IdTab (old_id_idx).xtyp,
           "type");
      end if;
    end Check_Formal_Parameter_List;
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
             severity => major);
    elsif sub_sub_params < forward_params then
      Error (CD, err_number_of_parameters_do_not_match,
             ": specification of " & A2S (id_current) & " has more parameters",
             severity => major);
    else
      Check_Formal_Parameter_List;
    end if;
  end Check_Subprogram_Spec_Body_Consistency;

  procedure Link_Forward_Declaration
    (CD         : in out Compiler_Data;
     old_id_idx :        Positive;
     new_id_idx :        Positive)
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
    --
    --  The linking is done!
    --
    CD.target.Mark_Spec_Body_Cross_References
      (spec_id => old_id_idx, body_id => new_id_idx);
  end Link_Forward_Declaration;

  procedure Check_Incomplete_Definitions
    (CD    : in out Co_Defs.Compiler_Data;
     level :        Defs.Nesting_Level)
  is
    id_index : Integer := CD.Blocks_Table (CD.Display (level)).Last_Id_Idx;
  begin
    --  Follow the chain of identifiers for given Level:
    while id_index /= No_Id loop
      if CD.IdTab (id_index).decl_kind = spec_unresolved
         and then CD.IdTab (id_index).entity /= entree
      then
        Error (CD, err_incomplete_declaration, A2S (CD.IdTab (id_index).name_with_case));
      end if;
      id_index := CD.IdTab (id_index).link;
    end loop;
  end Check_Incomplete_Definitions;

  function Nice_Image (item : IdTabEntry) return String is
  ((if item.decl_kind in Parameter_Kind then
      "parameter"
    else
      (case item.entity is
         when variable_object => "variable",
         when constant_object => "constant",
         when type_mark       => "type",
         when prozedure       => "procedure",
         when funktion        => "function",
         when paquetage       => "package",
         when tache           => "task",
         when others          => "item")) &
   " """ & A2S (item.name_with_case) & '"');

  procedure Mark_Read_and_Check_Read_before_Written
    (CD      : in out Compiler_Data;
     context : in     Flow_Context;
     item    : in out IdTabEntry)
  is
  begin
    Elevate_to_Maybe_or_Yes (item.is_read, context);
    if item.is_written_after_init = no      --  Not overwritten in a subprogram, nor in the above statements.
       and then item.is_initialized = none  --  Not initialized, even implicitly.
       and then context.level = item.lev    --  Not a within subprogram (uncertainty since call sequence is unknown).
    then
      Remark
        (CD,
         warn_read_but_not_written,
         Nice_Image (item) &
         (if context.is_in_cond_within_loop then
            --  We are not sure that the expression is
            --  evaluated in the first iteration of any loop.
            " may be"
          else
            --  We are sure that the expression is evaluated
            --  at the first iteration of all loops (if any).
            " is") &
         " read before it is ever written");
    end if;
  end Mark_Read_and_Check_Read_before_Written;

  procedure Check_Unused_or_Uninitialized_Items
    (CD    : in out Compiler_Data;
     level : in     Defs.Nesting_Level)
  is
    procedure Check_Item (item : IdTabEntry) is

      already_insulted : Natural := 0;

      procedure Remark_for_Declared_Item (diag : Compile_Diagnostic; text : String) is
        remark_made : Boolean;
      begin
        if already_insulted < 2 then
          Remark
            (CD,
             diag,
             text,
             location_method   => explicit,
             explicit_location => item.location,
             remark_made       => remark_made);
          --
          if remark_made then
            already_insulted := already_insulted + 1;
          end if;
        end if;
      end Remark_for_Declared_Item;

      procedure Handle_Variables_and_Parameters is
      begin
        case item.is_written_after_init is

          when no =>
            --  Not written.

            case item.is_initialized is

              when none =>
                if item.is_read = no
                  and then item.decl_kind /= param_out
                then
                  --  Neither read, not written, nor initialized.
                  --  Issue no warning. The note "-ru" will catch it as unused.
                  --  Special case: for an "out" parameter, we want a warning!
                  null;
                else
                  Remark_for_Declared_Item
                    (warn_read_but_not_written,
                     Nice_Image (item) &
                     (case item.is_read is
                        when no    => " is never written",  --  Case reached for an "out" parameter.
                        when maybe => " is never written, but is possibly read",
                        when yes   => " is read but never written") &
                     "");
                end if;

              when explicit =>
                Remark_for_Declared_Item
                  (note_constant_variable, Nice_Image (item) & " is not modified, could be declared constant");

              when implicit =>
                --  Implicitly initialized -> we don't care.
                null;
            end case;

          when maybe .. yes =>

            if item.is_read = no                    --  Maybe written after init., but not read.
              and then item.decl_kind /= param_out  --  Don't care about "out" param no being read.
            then
              Remark_for_Declared_Item (note_unused_item, Nice_Image (item) & " is never read");
            end if;

        end case;
      end Handle_Variables_and_Parameters;

      procedure Handle_Unused is
      begin
        --  Here we can have any explicit declaration
        --  (object, type, subprogram, ...)
        Remark_for_Declared_Item (note_unused_item, Nice_Image (item) & " is not referenced");
      end Handle_Unused;

    begin
      --  See table in "hac_work.xls", sheet "Remarks".

      if item.entity = variable_object then
        Handle_Variables_and_Parameters;
      end if;

      if item.entity /= alias and then not item.is_referenced then
        Handle_Unused;
      end if;
    end Check_Item;

    id_index : Integer := CD.Blocks_Table (CD.Display (level)).Last_Id_Idx;

  begin
    --  Follow the chain of identifiers for given Level:
    while id_index /= No_Id loop
      Check_Item (CD.IdTab (id_index));
      id_index := CD.IdTab (id_index).link;
    end loop;
  end Check_Unused_or_Uninitialized_Items;

  function Number_of_Parameters
    (CD         : in out Compiler_Data;
     id_idx     : in     Natural)
  return Natural
  is
    id_table_entry : IdTabEntry renames CD.IdTab (id_idx);
    block_idx : constant Integer := id_table_entry.block_or_pkg_ref;
  begin
    pragma Assert (id_table_entry.entity in prozedure | funktion);
    declare
      block : BTabEntry renames CD.Blocks_Table (block_idx);
    begin
      return
        (if block.First_Param_Id_Idx > block.Last_Param_Id_Idx then
           0
         else
           block.Last_Param_Id_Idx - block.First_Param_Id_Idx + 1);
    end;
  end Number_of_Parameters;

end HAC_Sys.Parser.Helpers;
