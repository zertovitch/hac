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
    Succ,
    Val,
    Value,
    VValue   --  Like Value but has a VString argument
  );

  procedure Scalar_Subtype_Attribute (
    CD             : in out Co_Defs.Compiler_Data;
    Level          : in     Defs.Nesting_level;
    FSys           : in     Defs.Symset;
    Typ_ID         : in     Co_Defs.IdTabEntry;
    Type_of_Result :    out Co_Defs.Exact_Subtyp
  )
  is
    use Co_Defs, Compiler.PCode_Emit, Defs, PCode, UErrors;
    attr_ID : constant String := To_String (CD.Id);
    attr : constant Attribute := Attribute'Value (attr_ID);
    --
    procedure First_Last_Discrete is
      discrete_value : HAC_Integer;
    begin
      if attr = First then
        discrete_value := Typ_ID.xTyp.Discrete_First;
      else
        discrete_value := Typ_ID.xTyp.Discrete_Last;
      end if;
      Emit_1 (CD, k_Push_Discrete_Literal, discrete_value);
    end First_Last_Discrete;
    --
    procedure First_Last is  --  RM 3.5 (12 ,13)
      RNum_Index : Natural;
    begin
      case Typ_ID.xTyp.TYP is
        when NOTYP =>
          null;  --  Already in error
        when Floats =>
          --  !! To do: floating point strict subtypes
          if attr = First then
            Helpers.Enter_or_find_Float (CD, HAC_Float'First, RNum_Index);
          else
            Helpers.Enter_or_find_Float (CD, HAC_Float'Last, RNum_Index);
          end if;
          Emit_1 (CD, k_Push_Float_Literal, Operand_2_Type (RNum_Index));
        when others =>
          if Discrete_Typ (Typ_ID.xTyp.TYP) then
            First_Last_Discrete;
          else
            Error (CD, err_attribute_prefix_invalid, attr_ID, severity => major);
          end if;
      end case;
      Type_of_Result := Typ_ID.xTyp;
    end First_Last;
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
      --  Argument is of the base type (S'Base).
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
      Type_of_Result := (String_Literals, 0, 0, 0);
    end Image;
    --
  begin
    Scanner.InSymbol (CD);  --  Consume the attribute name (First, Last, ...)
    case attr is
      when First | Last => First_Last;
      when Pred  | Succ => Pred_Succ;
      when Pos          => Pos;
      when Val          => Val;
      when Image        => Image;
      when others =>
        Error (CD, err_not_yet_implemented, "attribute " & attr_ID, major);
    end case;
  --  exception
  --    when Constraint_Error =>
  --      Error (CD, err_syntax_error, ": unknown attribute: " & attr_ID, major);
  end Scalar_Subtype_Attribute;

end HAC_Sys.Parser.Attributes;
