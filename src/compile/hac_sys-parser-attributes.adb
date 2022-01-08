with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Attributes is

  type Attribute is (
    First,
    Image,
    Last,
    Pos,
    Pred,
    Range_Attr,
    Succ,
    Val,
    Value
  );

  procedure Scalar_Subtype_Attribute (
    CD             : in out Co_Defs.Compiler_Data;
    Level          : in     Defs.Nesting_level;
    FSys           : in     Defs.Symset;
    Typ_ID_Index   : in     Natural;
    Type_of_Result :    out Co_Defs.Exact_Subtyp
  )
  is
    use Co_Defs, Compiler.PCode_Emit, Defs, PCode, UErrors;
    Typ_ID  : constant IdTabEntry := CD.IdTab (Typ_ID_Index);
    attr_ID : constant String     := To_String (CD.Id);
    attr    : Attribute;
    --
    procedure First_Last is  --  RM 3.5 (12, 13)
    begin
      case Typ_ID.xTyp.TYP is
        when NOTYP =>
          null;  --  Already in error
        when Floats =>
          --  !! To do: floating point strict subtypes
          Emit_Push_Float_Literal (CD,
            (if attr = First then HAC_Float'First else HAC_Float'Last)
          );
        when others =>
          if Discrete_Typ (Typ_ID.xTyp.TYP) then
            Emit_1 (CD, k_Push_Discrete_Literal,
              (if attr = First then Typ_ID.xTyp.Discrete_First else Typ_ID.xTyp.Discrete_Last)
            );
          else
            Error (CD, err_attribute_prefix_invalid, attr_ID, severity => major);
          end if;
      end case;
      Type_of_Result := Typ_ID.xTyp;
    end First_Last;
    --
    procedure Range_Attribute is  --  RM 3.5 (14)
    begin
      case Typ_ID.xTyp.TYP is
        when NOTYP =>
          null;  --  Already in error
        when Floats =>
          --  !! To do: floating point strict subtypes
          Emit_Push_Float_Literal (CD, HAC_Float'First);
          Emit_Push_Float_Literal (CD, HAC_Float'Last);
        when others =>
          if Discrete_Typ (Typ_ID.xTyp.TYP) then
            Emit_1 (CD, k_Push_Discrete_Literal, Typ_ID.xTyp.Discrete_First);
            Emit_1 (CD, k_Push_Discrete_Literal, Typ_ID.xTyp.Discrete_Last);
          else
            Error (CD, err_attribute_prefix_invalid, attr_ID, severity => major);
          end if;
      end case;
      Type_of_Result := (Scalar_Range, Typ_ID_Index, 0, 0);
    end Range_Attribute;
    --
    procedure Pred_Succ_Discrete is
      use type HAC_Integer;
    begin
      Emit_1 (CD, k_Push_Discrete_Literal, 1);
      if attr = Pred then
        --  !!  overflow check here if arg = hac_integer'first.
        Emit (CD, k_SUBTRACT_Integer);
        if Typ_ID.xTyp.Discrete_First > HAC_Integer'First then
          Emit_1 (CD, k_Check_Lower_bound, Typ_ID.xTyp.Discrete_First);
        end if;
      else
        --  !!  overflow check here if arg = hac_integer'first.
        Emit (CD, k_ADD_Integer);
        if Typ_ID.xTyp.Discrete_Last < HAC_Integer'Last then
          Emit_1 (CD, k_Check_Upper_bound, Typ_ID.xTyp.Discrete_Last);
        end if;
      end if;
    end Pred_Succ_Discrete;
    --
    procedure Pred_Succ is  --  RM 3.5 (22, 25)
      s_base, type_of_argument : Exact_Typ;
    begin
      Helpers.Need (CD, LParent, err_missing_an_opening_parenthesis);
      Expressions.Expression (CD, Level, FSys, type_of_argument);
      --  Argument is of the base type (S'Base).
      s_base := Exact_Typ (Typ_ID.xTyp);
      if s_base = type_of_argument then
        case Typ_ID.xTyp.TYP is
          when NOTYP =>
            null;  --  Already in error
          when Floats =>
            --  !! To do !!
            Error (CD, err_not_yet_implemented, "attribute " & attr_ID & " for this subtype", major);
          when others =>
            if Discrete_Typ (Typ_ID.xTyp.TYP) then
              Pred_Succ_Discrete;
            else
              Error (CD, err_attribute_prefix_invalid, attr_ID, major);
            end if;
        end case;
      else
        Helpers.Type_Mismatch (CD, err_parameter_types_do_not_match, type_of_argument, s_base);
      end if;
      Helpers.Need (CD, RParent, err_closing_parenthesis_missing);
      Type_of_Result := Typ_ID.xTyp;
    end Pred_Succ;
    --
    procedure Pos is  --  RM 3.5.5 (2)
      s_base, type_of_argument : Exact_Typ;
    begin
      if Discrete_Typ (Typ_ID.xTyp.TYP) then
        Helpers.Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expressions.Expression (CD, Level, FSys, type_of_argument);
        --  Argument is of the base type (S'Base).
        s_base := Exact_Typ (Typ_ID.xTyp);
        if s_base = type_of_argument then
          --  Just set the desired type, and that's it - no VM instruction!
          Type_of_Result := Helpers.Standard_Integer;
        else
          Helpers.Type_Mismatch (CD, err_parameter_types_do_not_match, type_of_argument, s_base);
        end if;
        Helpers.Need (CD, RParent, err_closing_parenthesis_missing);
      else
        Error (CD, err_attribute_prefix_must_be_discrete_type, attr_ID, major);
      end if;
    end Pos;
    --
    procedure Val is  --  RM 3.5.5 (5)
      type_of_argument : Exact_Typ;
    begin
      if Discrete_Typ (Typ_ID.xTyp.TYP) then
        Helpers.Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expressions.Expression (CD, Level, FSys, type_of_argument);
        if type_of_argument.TYP = Ints then
          --  Just set the desired type, and that's it - no VM instruction!
          Type_of_Result := Typ_ID.xTyp;
        else
          Helpers.Type_Mismatch
            (CD, err_parameter_types_do_not_match,
             type_of_argument, Exact_Typ (Helpers.Standard_Integer));
        end if;
        Helpers.Need (CD, RParent, err_closing_parenthesis_missing);
      else
        Error (CD, err_attribute_prefix_must_be_discrete_type, attr_ID, major);
      end if;
    end Val;
    --
    procedure Image is
      s_base, type_of_argument : Exact_Typ;
    begin
      Helpers.Need (CD, LParent, err_missing_an_opening_parenthesis);
      Expressions.Expression (CD, Level, FSys, type_of_argument);
      --  Argument is of the base type (S'Base). Translation: we forget the constraints here.
      s_base := Exact_Typ (Typ_ID.xTyp);
      if s_base = type_of_argument then
        case Typ_ID.xTyp.TYP is
          when NOTYP     => null;  --  Already in error
          when Ints      => Emit_Std_Funct (CD, SF_Image_Attribute_Ints);
          when Floats    => Emit_Std_Funct (CD, SF_Image_Attribute_Floats);
          when Bools     => Emit_Std_Funct (CD, SF_Image_Attribute_Bools);
          when Chars     => Emit_Std_Funct (CD, SF_Image_Attribute_Chars);
          when Durations => Emit_Std_Funct (CD, SF_Image_Attribute_Durs);
          when Enums     =>
            --  The enumeration items' declarations follow the type name
            --  For example: `type Enum is (a, b, c)`, we send the index
            --  of the first item, `a`, in the identifier table.
            Emit_Std_Funct (CD, SF_Image_Attribute_Enums, Operand_1_Type (Typ_ID.xTyp.Ref + 1));
          when others =>
            Error (CD, err_attribute_prefix_invalid, attr_ID, major);
        end case;
      else
        Helpers.Type_Mismatch (CD, err_parameter_types_do_not_match, type_of_argument, s_base);
      end if;
      Helpers.Need (CD, RParent, err_closing_parenthesis_missing);
      Type_of_Result := (Strings_as_VStrings, 0, 0, 0);
    end Image;
    --
    procedure Value is
      type_of_argument : Exact_Typ;
      use Helpers;
    begin
      Helpers.Need (CD, LParent, err_missing_an_opening_parenthesis);
      Expressions.Expression (CD, Level, FSys, type_of_argument);
      --  Argument is of the base type (S'Base).
      if type_of_argument.TYP = String_Literals then
        Emit_Std_Funct (CD, SF_Literal_to_VString);
      elsif type_of_argument.TYP = Strings_as_VStrings then
        null;
      elsif Is_Char_Array (CD, type_of_argument) then
        Emit_Std_Funct (CD,
          SF_String_to_VString,
          Operand_1_Type (CD.Arrays_Table (type_of_argument.Ref).Array_Size)
        );
      else
      Type_Mismatch (
        CD,
        err_parameter_types_do_not_match,
        Found    => type_of_argument,
        Expected => Str_Lit_Set or Str_as_VStr_Set or Arrays_Set
      );
      end if;
      case Typ_ID.xTyp.TYP is
        when NOTYP     => null;  --  Already in error
        when Ints      =>
          Emit_Std_Funct (CD, SF_Value_Attribute_Ints);
          Type_of_Result := (Ints, 0, 0, 0);
        when Floats    =>
          Emit_Std_Funct (CD, SF_Value_Attribute_Floats);
          Type_of_Result := (Floats, 0, 0, 0);
        when Bools     =>
          Emit_Std_Funct (CD, SF_Value_Attribute_Bools);
          Type_of_Result := (Bools, 0, 0, 0);
        when Chars     =>
          Emit_Std_Funct (CD, SF_Value_Attribute_Chars);
          Type_of_Result := (Chars, 0, 0, 0);
        when Durations =>
          Emit_Std_Funct (CD, SF_Value_Attribute_Durs);
          Type_of_Result := (Durations, 0, 0, 0);
        when Enums     =>
          Emit_Std_Funct (CD, SF_Value_Attribute_Enums, Operand_1_Type (Typ_ID.xTyp.Ref));
          Type_of_Result := Typ_ID.xTyp;
        when others =>
          Error (CD, err_attribute_prefix_invalid, attr_ID, major);
      end case;
      Helpers.Need (CD, RParent, err_closing_parenthesis_missing);
    end Value;
    --
  begin
    if attr_ID = "RANGE" then
      attr := Range_Attr;
    else
      attr := Attribute'Value (attr_ID);
    end if;
    Scanner.InSymbol (CD);  --  Consume the attribute name (First, Last, ...)
    case attr is
      when First | Last => First_Last;       --  RM 3.5 (12, 13)
      when Range_Attr   => Range_Attribute;  --  RM 3.5 (14)
      when Pred  | Succ => Pred_Succ;
      when Pos          => Pos;
      when Val          => Val;
      when Image        => Image;
      when Value        => Value;
    end case;
  exception
    when Constraint_Error =>
      Error (CD, err_syntax_error, ": unknown attribute: " & attr_ID, major);
  end Scalar_Subtype_Attribute;

end HAC_Sys.Parser.Attributes;
