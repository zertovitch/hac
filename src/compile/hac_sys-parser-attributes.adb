with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Ranges,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Attributes is

  type Attribute is (
    First,
    Image,
    Last,
    Length,
    Pos,
    Pred,
    Range_Attr,
    Succ,
    Val,
    Value
  );

  procedure Which_Attribute (CD : in out Co_Defs.Compiler_Data; attr : out Attribute) is
    use Co_Defs, Defs, Errors;
    attr_ID : constant String := A2S (CD.Id);
  begin
    attr := (if attr_ID = "RANGE" then Range_Attr else Attribute'Value (attr_ID));
    Scanner.InSymbol (CD);  --  Consume the attribute name (First, Last, ...)
  exception
    when Constraint_Error =>
      Error (CD, err_syntax_error, ": unknown attribute: " & attr_ID, major);
  end Which_Attribute;

  procedure Array_Subtype_Attribute (
    CD                : in out Co_Defs.Compiler_Data;
    Level             : in     Defs.Nesting_level;
    FSys              : in     Defs.Symset;
    Array_Index       : in     Natural;
    attr              : in     Attribute;
    xSubtyp_of_Result :    out Co_Defs.Exact_Subtyp
  )
  is
    use Co_Defs, Defs, Helpers, Errors;
    A : ATabEntry := CD.Arrays_Table (Array_Index);
    Low, High : Index;
    N : Constant_Rec;
    use Compiler.PCode_Emit, Expressions, PCode, Scanner;
    use type HAC_Integer;
  begin
    N.I := 1;
    if CD.Sy = LParent then
      InSymbol (CD);
      Static_Scalar_Expression (CD, Level, FSys + RParent, N);
      if N.TP.TYP /= Ints then
        Error (CD, err_parameter_must_be_Integer, severity => major);
      end if;
      Need (CD, RParent, err_closing_parenthesis_missing);
    end if;
    if N.I < 1 then
      Error (CD, err_invalid_dimension_number, "minimum is 1", major);
    end if;
    --
    Jump_to_next_Dimension :
    for Skip_Dim in 2 .. N.I loop
      if A.Element_xTyp.TYP = Arrays then
        A := CD.Arrays_Table (A.Element_xTyp.Ref);
      else
        Error (CD, err_invalid_dimension_number, "maximum is" &
          HAC_Integer'Image (Skip_Dim - 1), major);
      end if;
    end loop Jump_to_next_Dimension;
    --
    Low  := Index (A.Index_xTyp.Discrete_First);
    High := Index (A.Index_xTyp.Discrete_Last);
    case attr is
      when First =>       --  RM 3.6.2 (3, 4)
        Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (Low));
        xSubtyp_of_Result := A.Index_xTyp;
      when Last =>        --  RM 3.6.2 (5, 6)
        Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (High));
        xSubtyp_of_Result := A.Index_xTyp;
      when Range_Attr =>  --  RM 3.6.2 (7, 8)
        Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (Low));
        Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (High));
        xSubtyp_of_Result := A.Index_xTyp;
        xSubtyp_of_Result.Is_Range := True;
      when Length =>      --  RM 3.6.2 (9, 10)
        Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (High - Low + 1));
        Construct_Root (xSubtyp_of_Result, Ints);
      when others =>
        Error (CD, err_syntax_error, ": attribute not defined for this type", major);
    end case;
  end Array_Subtype_Attribute;

  procedure Image_Attribute  --  S'Image (...) or X'Image
    (CD                : in out Co_Defs.Compiler_Data;
     S                 : in     Co_Defs.Exact_Subtyp;
     xSubtyp_of_Result :    out Co_Defs.Exact_Subtyp)
  is
    use Co_Defs, Compiler.PCode_Emit, Defs, Errors, PCode;
  begin
    --  The value to get an image from is assumed to be on top
    --  of the stack. The appropriate built-in function will replace
    --  it by its image as a string (internal type: Strings_as_VStrings).
    case S.TYP is
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
        Emit_Std_Funct (CD, SF_Image_Attribute_Enums, Operand_1_Type (S.Ref + 1));
      when others =>
        Error (CD, err_attribute_prefix_invalid, "Image", major);
    end case;
    --  The result subtype is a VString disguised as a String.
    Construct_Root (xSubtyp_of_Result, Strings_as_VStrings);
  end Image_Attribute;

  procedure Object_Attribute (
    CD                : in out Co_Defs.Compiler_Data;
    Level             : in     Defs.Nesting_level;
    FSys              : in     Defs.Symset;
    Object_xSubtyp    : in     Co_Defs.Exact_Subtyp;
    LC_before_Object  : in     Integer;
    xSubtyp_of_Result :    out Co_Defs.Exact_Subtyp
  )
  is
    use Defs, Helpers, Errors;
    attr : Attribute;
  begin
    Which_Attribute (CD, attr);
    case attr is
      when Image =>  --  X'Image   (Ada 2022)
        Image_Attribute (CD, Object_xSubtyp, xSubtyp_of_Result);
      when others =>
        if Arrays_Set (Object_xSubtyp.TYP) then
          --  Forget all the code emitted for selecting the array variable.
          --  It can be a complex thing like `a (i * 2).loc_x (3)` !
          CD.LC := LC_before_Object;
          Array_Subtype_Attribute (CD, Level, FSys + RParent, Object_xSubtyp.Ref, attr, xSubtyp_of_Result);
        else
          Error (CD, err_syntax_error, ": attribute not defined for this object", major);
        end if;
    end case;
  end Object_Attribute;

  procedure Subtype_Attribute (
    CD                : in out Co_Defs.Compiler_Data;
    Level             : in     Defs.Nesting_level;
    FSys              : in     Defs.Symset;
    Typ_ID_Index      : in     Natural;
    xSubtyp_of_Result :    out Co_Defs.Exact_Subtyp
  )
  is
    use Co_Defs, Defs, Helpers, Errors;
    Typ_ID : IdTabEntry renames CD.IdTab (Typ_ID_Index);
    S : Exact_Subtyp renames Typ_ID.xtyp;
    attr_ID : constant String := A2S (CD.Id);
    attr : Attribute;
    --
    procedure Scalar_Subtype_Attribute is
      use Compiler.PCode_Emit, PCode;
      --
      procedure First_Last is  --  S'First, S'Last: RM 3.5 (12, 13)
      begin
        case S.TYP is
          when NOTYP =>
            null;  --  Already in error
          when Floats =>
            --  !! To do: floating point strict subtypes
            if attr = First then
              Emit (CD, k_Push_Float_First);
            else
              Emit (CD, k_Push_Float_Last);
            end if;
          when others =>
            if Discrete_Typ (S.TYP) then
              Emit_1 (CD, k_Push_Discrete_Literal,
                (if attr = First then S.Discrete_First else S.Discrete_Last)
              );
            else
              Error (CD, err_attribute_prefix_invalid, attr_ID, severity => major);
            end if;
        end case;
        xSubtyp_of_Result := S;
        if Discrete_Typ (S.TYP) then
          Ranges.Set_Singleton_Range
            (xSubtyp_of_Result, (if attr = First then S.Discrete_First else S.Discrete_Last));
        end if;
      end First_Last;
      --
      procedure Range_Attribute is  --  S'Range: RM 3.5 (14)
      begin
        case S.TYP is
          when NOTYP =>
            null;  --  Already in error
          when Floats =>
            --  !! To do: floating point strict subtypes
            Emit_Push_Float_Literal (CD, HAC_Float'First);
            Emit_Push_Float_Literal (CD, HAC_Float'Last);
          when others =>
            if Discrete_Typ (S.TYP) then
              Emit_1 (CD, k_Push_Discrete_Literal, S.Discrete_First);
              Emit_1 (CD, k_Push_Discrete_Literal, S.Discrete_Last);
            else
              Error (CD, err_attribute_prefix_invalid, attr_ID, severity => major);
            end if;
        end case;
        xSubtyp_of_Result          := S;
        xSubtyp_of_Result.Is_Range := True;
      end Range_Attribute;
      --
      procedure Pred_Succ_Discrete is
        use type HAC_Integer;
      begin
        Emit_1 (CD, k_Push_Discrete_Literal, 1);
        if attr = Pred then
          --  !!  overflow check here if arg = hac_integer'first.
          Emit (CD, k_SUBTRACT_Integer);
          if S.Discrete_First > HAC_Integer'First then
            Emit_3
              (CD, k_Check_Lower_Bound,
               S.Discrete_First, Typen'Pos (S.TYP), Operand_3_Type (S.Ref));
          end if;
        else
          --  !!  overflow check here if arg = hac_integer'first.
          Emit (CD, k_ADD_Integer);
          if S.Discrete_Last < HAC_Integer'Last then
            Emit_3
              (CD, k_Check_Upper_Bound,
               S.Discrete_Last, Typen'Pos (S.TYP), Operand_3_Type (S.Ref));
          end if;
        end if;
      end Pred_Succ_Discrete;
      --
      procedure Pred_Succ is  --  S'Pred (...), S'Succ (...): RM 3.5 (22, 25)
        s_base : Exact_Typ;
        type_of_argument : Exact_Subtyp;
      begin
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expressions.Expression (CD, Level, FSys + RParent, type_of_argument);
        --  Argument is of the base type (S'Base).
        s_base := Exact_Typ (S);
        if s_base = Exact_Typ (type_of_argument) then
          case S.TYP is
            when NOTYP =>
              null;  --  Already in error
            when Floats =>
              --  !! To do !!
              Error (CD, err_not_yet_implemented, "attribute " & attr_ID & " for this subtype", major);
            when others =>
              if Discrete_Typ (S.TYP) then
                Pred_Succ_Discrete;
              else
                Error (CD, err_attribute_prefix_invalid, attr_ID, major);
              end if;
          end case;
        else
          Type_Mismatch (CD, err_parameter_types_do_not_match, type_of_argument, s_base);
        end if;
        Need (CD, RParent, err_closing_parenthesis_missing);
        xSubtyp_of_Result := S;
      end Pred_Succ;
      --
      procedure Pos is  --  S'Pos (...): RM 3.5.5 (2)
        s_base : Exact_Typ;
        type_of_argument : Exact_Subtyp;
      begin
        if Discrete_Typ (S.TYP) then
          Need (CD, LParent, err_missing_an_opening_parenthesis);
          Expressions.Expression (CD, Level, FSys + RParent, type_of_argument);
          --  Argument is of the base type (S'Base).
          s_base := Exact_Typ (S);
          if s_base = Exact_Typ (type_of_argument) then
            --  Just set the desired type, and that's it - no VM instruction!
            xSubtyp_of_Result := Standard_Integer;
          else
            Type_Mismatch (CD, err_parameter_types_do_not_match, type_of_argument, s_base);
          end if;
          Need (CD, RParent, err_closing_parenthesis_missing);
        else
          Error (CD, err_attribute_prefix_must_be_discrete_type, attr_ID, major);
        end if;
      end Pos;
      --
      procedure Val is  --  S'Val (...): RM 3.5.5 (5)
        type_of_argument : Exact_Subtyp;
      begin
        if Discrete_Typ (S.TYP) then
          Helpers.Need (CD, LParent, err_missing_an_opening_parenthesis);
          Expressions.Expression (CD, Level, FSys + RParent, type_of_argument);
          if type_of_argument.TYP = Ints then
            --  Just set the desired subtype, and that's it - no VM instruction!
            xSubtyp_of_Result := S;
          else
            Helpers.Type_Mismatch
              (CD, err_parameter_types_do_not_match, type_of_argument, Helpers.Standard_Integer);
          end if;
          Helpers.Need (CD, RParent, err_closing_parenthesis_missing);
        else
          Error (CD, err_attribute_prefix_must_be_discrete_type, attr_ID, major);
        end if;
      end Val;
      --
      procedure Image is  --  S'Image (...)
        type_of_argument : Exact_Subtyp;
        --  Argument of the function is of the base type, that is: S'Base.
        --  Translation: we forget the subtype constraints here.
        s_base : constant Exact_Typ := Exact_Typ (S);
      begin
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expressions.Expression (CD, Level, FSys + RParent, type_of_argument);
        if s_base = Exact_Typ (type_of_argument) then
          Image_Attribute (CD, S, xSubtyp_of_Result);
        else
          Type_Mismatch (CD, err_parameter_types_do_not_match, type_of_argument, s_base);
        end if;
        Need (CD, RParent, err_closing_parenthesis_missing);
      end Image;
      --
      procedure Value is
        type_of_argument : Exact_Subtyp;
      begin
        Need (CD, LParent, err_missing_an_opening_parenthesis);
        Expressions.Expression (CD, Level, FSys + RParent, type_of_argument);
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
        case S.TYP is
          when NOTYP     => null;  --  Already in error
          when Ints      =>
            Emit_Std_Funct (CD, SF_Value_Attribute_Ints);
            Construct_Root (xSubtyp_of_Result, Ints);
          when Floats    =>
            Emit_Std_Funct (CD, SF_Value_Attribute_Floats);
            Construct_Root (xSubtyp_of_Result, Floats);
          when Bools     =>
            Emit_Std_Funct (CD, SF_Value_Attribute_Bools);
            Construct_Root (xSubtyp_of_Result, Bools);
          when Chars     =>
            Emit_Std_Funct (CD, SF_Value_Attribute_Chars);
            Construct_Root (xSubtyp_of_Result, Chars);
          when Durations =>
            Emit_Std_Funct (CD, SF_Value_Attribute_Durs);
            Construct_Root (xSubtyp_of_Result, Durations);
          when Enums     =>
            Emit_Std_Funct (CD, SF_Value_Attribute_Enums, Operand_1_Type (S.Ref));
            xSubtyp_of_Result := S;
          when others =>
            Error (CD, err_attribute_prefix_invalid, attr_ID, major);
        end case;
        Need (CD, RParent, err_closing_parenthesis_missing);
      end Value;
      --
    begin
      case attr is
        when First | Last => First_Last;       --  RM 3.5 (12, 13)
        when Range_Attr   => Range_Attribute;  --  RM 3.5 (14)
        when Pred  | Succ => Pred_Succ;
        when Pos          => Pos;
        when Val          => Val;
        when Image        => Image;
        when Value        => Value;
        when others =>
          Error (
            CD,
            err_syntax_error,
            ": attribute not defined for this type",
            major
          );
      end case;
    end Scalar_Subtype_Attribute;
    --
  begin
    pragma Assert (Typ_ID.entity = TypeMark);
    --
    Which_Attribute (CD, attr);
    if Scalar_Set (S.TYP) then
      Scalar_Subtype_Attribute;
    elsif Arrays_Set (S.TYP) then
      Array_Subtype_Attribute (CD, Level, FSys + RParent, S.Ref, attr, xSubtyp_of_Result);
    else
      Error (CD, err_syntax_error,
        ": no attribute defined for this type: " &
        A2S (Typ_ID.name_with_case), major
      );
    end if;
  end Subtype_Attribute;

end HAC_Sys.Parser.Attributes;
