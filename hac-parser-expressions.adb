with HAC.Parser.Calls;                  use HAC.Parser.Calls;
with HAC.Parser.Helpers;                use HAC.Parser.Helpers;
with HAC.Parser.Standard_Subprograms;   use HAC.Parser.Standard_Subprograms;
with HAC.PCode;                         use HAC.PCode;
with HAC.Scanner;                       use HAC.Scanner;
with HAC.UErrors;                       use HAC.UErrors;

package body HAC.Parser.Expressions is

  ------------------------------------------------------------------
  ---------------------------------------------------------Selector-
  procedure Selector (
    CD    : in out Compiler_Data;
    Level :        Integer;
    FSys  :        Symset;
    V     : in out Exact_Typ
  )
  is
    --
    procedure Record_Field_Selector is
      Field_Offset, Field_Id : Integer;
    begin
      if V.TYP = Records then
        Field_Id := CD.Blocks_Table (V.Ref).Last_Id_Idx;
        CD.IdTab (0).Name := CD.Id;
        while CD.IdTab (Field_Id).Name /= CD.Id loop  --  Search field identifier
          Field_Id := CD.IdTab (Field_Id).Link;
        end loop;
        if Field_Id = No_Id then
          Error (CD, err_undefined_identifier, stop_on_error => True);
        end if;
        V.TYP        := CD.IdTab (Field_Id).TYP;
        V.Ref        := CD.IdTab (Field_Id).Ref;
        Field_Offset := CD.IdTab (Field_Id).Adr;
        if Field_Offset /= 0 then
          Emit1 (CD, k_Record_Field_Offset, Field_Offset);
        end if;
      else
        Error (CD, err_var_with_field_selector_must_be_record);
      end if;
      InSymbol (CD);
    end Record_Field_Selector;
    --
    procedure Array_Coordinates_Selector is
      Array_Index_Expr : Exact_Typ;  --  Evaluation of "i", "j+7", "k*2" in "a (i, j+7, k*2)".
      Arr_Tab_Index : Integer;       --  Index in the table of all arrays definitions.
    begin
      loop
        InSymbol (CD);  --  Consume '(' or ',' symbol.
        Expression (CD, Level, FSys + Comma_RParent + RBrack, Array_Index_Expr);
        if V.TYP = Arrays then
          Arr_Tab_Index := V.Ref;
          if CD.Arrays_Table (Arr_Tab_Index).Index_xTyp /= Array_Index_Expr then
            Error (CD, err_illegal_array_subscript);
          elsif CD.Arrays_Table (Arr_Tab_Index).Element_Size = 1 then
            Emit1 (CD, k_Array_Index_Element_Size_1, Arr_Tab_Index);
          else
            Emit1 (CD, k_Array_Index, Arr_Tab_Index);
          end if;
          V := CD.Arrays_Table (Arr_Tab_Index).Element_xTyp;
        else
          Error (CD, err_indexed_variable_must_be_an_array);
        end if;
        exit when CD.Sy /= Comma;
      end loop;
    end Array_Coordinates_Selector;
    --
    err : Compile_Error;
  begin
    pragma Assert (Selector_Symbol_Loose (CD.Sy));  --  '.' or '(' or (wrongly) '['
    loop
      if CD.Sy = Period then
        InSymbol (CD);  --  Consume '.' symbol.
        if CD.Sy = IDent then
          Record_Field_Selector;
        else
          Error (CD, err_identifier_missing);
        end if;
      else
        if CD.Sy = LBrack then  --  '['
          --  Common mistake by Pascal, Python or R programmers.
          Error (CD, err_left_bracket_instead_of_parenthesis);
        end if;
        Array_Coordinates_Selector;
        if CD.Sy = RBrack then  --  ']' : same kind of mistake as for '[' ...
          Error (CD, err_right_bracket_instead_of_parenthesis);
          InSymbol (CD);
        else
          Need (CD, RParent, err_closing_parenthesis_missing);
        end if;
      end if;
      exit when not Selector_Symbol_Loose (CD.Sy);
    end loop;
    --
    if FSys = Semicolon_Set then
      err := err_semicolon_missing;
    else
      err := err_incorrectly_used_symbol;
    end if;
    Test (CD, FSys, Empty_Symset, err);
  end Selector;

  ------------------------------------------------------------------
  -------------------------------------------------------Expression-
  procedure Expression (
    CD    : in out Compiler_Data;
    Level :        Integer;
    FSys  :        Symset;
    X     :    out Exact_Typ
  )
  is
    Y  : Exact_Typ;
    OP : KeyWSymbol;

    procedure Simple_Expression (FSys : Symset; X : out Exact_Typ) is
      Y  : Exact_Typ;
      OP : KeyWSymbol;

      procedure Term (FSys : Symset; X : out Exact_Typ) is
        Y  : Exact_Typ;
        OP : KeyWSymbol;

        procedure Factor (FSys : Symset; X : out Exact_Typ) is
          Ident_Index : Integer;
          F           : Opcode;
          err         : Compile_Error;

          procedure Type_Conversion is  --  Ada RM 4.6
            kind    :          Type_Conversion_Kind := Unknown;
            Type_Id : constant String               := To_String (CD.Id);
          begin
            Need (CD, LParent, err_missing_an_opening_parenthesis);
            if Type_Id = HAC_Float_Name then
              kind := To_Float;
            elsif Type_Id = HAC_Integer_Name then
              kind := To_Integer;
            end if;
            Expression (CD, Level, FSys + RParent, X);
            case kind is
              when To_Float =>
                case X.TYP is
                  when Floats =>
                    null;  --  !!  Emit warning: "already float"
                  when Ints =>
                    Emit1 (CD, k_Integer_to_Float, 0);
                  when others =>
                    Argument_Type_Not_Supported (CD);
                end case;
                X.TYP := Floats;
              when To_Integer =>
                case X.TYP is
                  when Floats =>  --  Rounding to closest integer (Ada RM 4.6 (33)).
                    Emit_Std_Funct (CD, SF_Round_Float_to_Int);
                  when Ints =>
                    null;  --  !!  Emit warning: "already integer"
                  when others =>
                    Argument_Type_Not_Supported (CD);
                end case;
                X.TYP := Ints;
              when Unknown =>
                Error (CD, err_type_conversion_not_supported, "no support for target type " & Type_Id);
            end case;
            Need (CD, RParent, err_closing_parenthesis_missing);
          end Type_Conversion;

        begin  --  Factor
          X := (TYP => NOTYP, Ref => 0);
          Test (CD, Factor_Begin_Symbol + StrCon, FSys, err_factor_unexpected_symbol);
          if CD.Sy = StrCon then
            X.TYP := String_Literals;
            Emit1 (CD, k_Load_Discrete_Literal, CD.SLeng);  --  String Literal Length
            Emit1 (CD, k_Load_Discrete_Literal, CD.INum);   --  Index To String IdTab
            InSymbol (CD);
          end if;
          while Factor_Begin_Symbol (CD.Sy) loop
            case CD.Sy is
              when IDent =>
                Ident_Index := Locate_Identifier (CD, CD.Id, Level, stop_on_error => True);
                InSymbol (CD);
                exit when Ident_Index = No_Id;  --  Id not found, error already issued by Locate_Identifier
                declare
                  r : IdTabEntry renames CD.IdTab (Ident_Index);
                begin
                  case r.Obj is
                    when Declared_Number_or_Enum_Item =>
                      X := (TYP => r.TYP, Ref => r.Ref);
                      if X.TYP = Floats then
                        --  Address is an index in the float constants table.
                        Emit1 (CD, k_Load_Float_Literal, r.Adr);
                      else
                        --  Here the address is actually the immediate (discrete) value.
                        Emit1 (CD, k_Load_Discrete_Literal, r.Adr);
                      end if;
                      --
                    when Variable =>
                      X := (TYP => r.TYP, Ref => r.Ref);
                      if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
                        if r.Normal then
                          F := k_Load_Address;
                        else
                          F := k_Push_Value;
                        end if;
                        Emit2 (CD, F, r.LEV, r.Adr);
                        Selector (CD, Level, FSys, X);
                        if Standard_or_Enum_Typ (X.TYP) then
                          Emit (CD, k_Case_34);
                        end if;
                      else
                        if Standard_or_Enum_Typ (X.TYP) then
                          if r.Normal then
                            F := k_Push_Value;
                          else
                            F := k_Push_Indirect_Value;
                          end if;
                        elsif r.Normal then
                          F := k_Load_Address;
                        else
                          F := k_Push_Value;
                        end if;
                        Emit2 (CD, F, r.LEV, r.Adr);
                      end if;
                      --
                    when TypeMark =>
                      Type_Conversion;
                      --
                    when Prozedure =>
                      Error (CD, err_expected_constant_function_variable_or_subtype);
                      --
                    when Funktion =>
                      X.TYP := r.TYP;
                      if r.LEV = 0 then
                        Standard_Function (CD, Level, FSys, Ident_Index, SF_Code'Val (r.Adr), X);
                      else
                        Subprogram_or_Entry_Call (CD, Level, FSys, Ident_Index, CallSTDP);
                      end if;
                      --
                    when others =>
                      null;
                  end case;
                end;
                --
              when CharCon | IntCon | FloatCon =>
                if CD.Sy = FloatCon then
                  X.TYP := Floats;
                  declare
                    RNum_Index : Natural;
                  begin
                    Enter_or_find_Float (CD, CD.RNum, RNum_Index);
                    Emit1 (CD, k_Load_Float_Literal, RNum_Index);
                  end;
                else
                  if CD.Sy = CharCon then
                    X.TYP := Chars;
                  else
                    X.TYP := Ints;
                  end if;
                  Emit1 (CD, k_Load_Discrete_Literal, CD.INum);
                end if;
                X.Ref := 0;
                InSymbol (CD);
                --
              when LParent =>    --  (
                InSymbol (CD);
                Expression (CD, Level, FSys + RParent, X);
                Need (CD, RParent, err_closing_parenthesis_missing);
                --
              when NOT_Symbol =>
                InSymbol (CD);
                Factor (FSys, X);
                if X.TYP = Bools then
                  Emit (CD, k_NOT_Boolean);
                elsif X.TYP /= NOTYP then
                  Error (CD, err_resulting_type_should_be_Boolean);
                end if;
                --
              when others =>
                null;
            end case;
            --
            if FSys = Semicolon_Set then
              err := err_semicolon_missing;
            else
              err := err_incorrectly_used_symbol;
            end if;
            Test (CD, FSys, Factor_Begin_Symbol, err);
          end loop;
        end Factor;

      begin  --  Term
        Factor (FSys + FactorZ, X);
        --
        --  We collect here eventual factors: a {* b}
        --
        while FactorZ (CD.Sy) loop
          OP := CD.Sy;
          InSymbol (CD);
          Factor (FSys + FactorZ, Y);
          if X.TYP = NOTYP or Y.TYP = NOTYP then
            null;  --  Something is already wrong at this point; nothing to check or emit.
          else
            case OP is
              when Times =>     --  *
                if X.TYP in Numeric_Typ and then Y.TYP in Numeric_Typ then
                  if X.TYP = Y.TYP then
                    Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                  else
                    Forbid_Type_Coercion (CD, "for this standard operator, types must be the same");
                  end if;
                elsif X.TYP = Ints and then Y.TYP = Chars then     --  N * Some_Char
                  Emit_Std_Funct (CD, SF_Int_Times_Char);
                  X.TYP := VStrings;
                elsif X.TYP = Ints and then Y.TYP = VStrings then  --  N * Some_VString
                  Emit_Std_Funct (CD, SF_Int_Times_VStr);
                  X.TYP := VStrings;
                else
                  Error (CD, err_operator_not_defined_for_types);
                end if;
              when Divide =>    --  /
                if X.TYP in Numeric_Typ and then X.TYP = Y.TYP then
                  Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                else
                  if X.TYP = Ints then
                    Forbid_Type_Coercion (CD, "left operand's type is integer, right operand's isn't");
                    Emit1 (CD, k_Integer_to_Float, 1);  --  NB: this assumed Y.TYP was Floats!
                    X.TYP := Floats;
                  end if;
                  if Y.TYP = Ints then
                    Forbid_Type_Coercion (CD, "right operand's type is integer, left operand's isn't");
                    Emit1 (CD, k_Integer_to_Float, 0);  --  NB: this assumed Y.TYP was Floats!
                    Y.TYP := Floats;
                  end if;
                  Error (CD, err_illegal_type_for_arithmetic_expression);
                  X.TYP := NOTYP;
                end if;
              when AND_Symbol =>
                if X.TYP = Bools and Y.TYP = Bools then
                  Emit (CD, k_AND_Boolean);
                else
                  Error (CD, err_resulting_type_should_be_Boolean);
                  X.TYP := NOTYP;
                end if;
              when MOD_Symbol =>
                if X.TYP = Ints and Y.TYP = Ints then
                  Emit (CD, k_MOD_Integer);
                else
                  Error (CD, err_mod_requires_integer_arguments);
                  X.TYP := NOTYP;
                end if;
              when Power =>
                if X.TYP in Numeric_Typ and then X.TYP = Y.TYP then
                  Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                elsif X.TYP = Floats and Y.TYP = Ints then
                  Emit (CD, k_Power_Float_Integer);
                else
                  Error (CD, err_invalid_power_operands);
                end if;
              when others =>
                raise Internal_error with "Unknown operator in Term";
            end case;
          end if;
        end loop;
      end Term;

    begin  --  Simple_Expression
      if Plus_Minus (CD.Sy) then
        --
        --  Unary + , -      RM 4.5 (5), 4.4 (4)
        --
        OP := CD.Sy;
        InSymbol (CD);
        Term (FSys + Plus_Minus, X);
        if OP = Plus and then X.TYP = String_Literals then  --  +"Hello"
          Emit_Std_Funct (CD, SF_Literal_to_VString);
          X.TYP := VStrings;
        elsif X.TYP not in Numeric_Typ then
          Error (CD, err_illegal_type_for_arithmetic_expression);
        elsif OP = Minus then
          Emit_Unary_Minus (CD, X.TYP);
        end if;
      else
        Term (FSys + Binary_Adding_Operators, X);
      end if;
      --
      --  We collect here eventual terms: a {+ b}      RM 4.4 (4)
      --
      while Binary_Adding_Operators (CD.Sy) loop
        OP := CD.Sy;
        InSymbol (CD);
        Term (FSys + Binary_Adding_Operators, Y);
        if X.TYP = NOTYP or Y.TYP = NOTYP then
          null;  --  Something is already wrong at this point; nothing to check or emit.
        else
          case OP is
            when OR_Symbol =>
              if X.TYP = Bools and Y.TYP = Bools then
                Emit (CD, k_OR_Boolean);
              else
                Error (CD, err_resulting_type_should_be_Boolean);
                X.TYP := NOTYP;
              end if;
            when XOR_Symbol =>
              if X.TYP = Bools and Y.TYP = Bools then
                Emit (CD, k_XOR_Boolean);
              else
                Error (CD, err_resulting_type_should_be_Boolean);
                X.TYP := NOTYP;
              end if;
            when Plus | Minus =>
              if X.TYP in Numeric_Typ then
                if X.TYP = Y.TYP then
                  Emit_Arithmetic_Binary_Instruction (CD, OP, X.TYP);
                else
                  Forbid_Type_Coercion (CD, "for this standard operator, types must be the same");
                end if;
              else
                Error (CD, err_operator_not_defined_for_types);
              end if;
            when Ampersand_Symbol =>  --  Concatenation. RM: Unbounded_String.
              if X.TYP = VStrings and Y.TYP = VStrings then            --  v & v     RM A.4.5 (15)
                Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
              elsif X.TYP = VStrings and Y.TYP = String_Literals then  --  v & "x"   RM A.4.5 (16)
                --  Y is on top of the stack, we turn it into a VString.
                Emit_Std_Funct (CD, SF_Literal_to_VString);
                --  Now we concatenate both VStrings.
                Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
              elsif X.TYP = String_Literals and Y.TYP = VStrings then  --  "x" & v   RM A.4.5 (17)
                Emit_Std_Funct (CD, SF_LStr_VString_Concat);
                X.TYP := VStrings;
              elsif X.TYP = VStrings and Y.TYP = Chars then            --  v & 'x'   RM A.4.5 (18)
                Emit_Std_Funct (CD, SF_VString_Char_Concat);
              elsif X.TYP = Chars and Y.TYP = VStrings then            --  'x' & v   RM A.4.5 (19)
                Emit_Std_Funct (CD, SF_Char_VString_Concat);
                X.TYP := VStrings;
              else
                Error (CD, err_operator_not_defined_for_types);
              end if;
            when others =>  --  Doesn't happen: Binary_Adding_Operators(OP) is True.
              null;
          end case;
        end if;
      end loop;
    end Simple_Expression;

  begin  --  Expression
    Simple_Expression (FSys + Comparison_Operator_Set, X);
    --
    --  We collect here an eventual comparison: a {= b}
    --
    if CD.Sy in Comparison_Operator then
      OP := CD.Sy;
      InSymbol (CD);
      Simple_Expression (FSys, Y);
      if X.TYP = Ints and Y.TYP = Floats then
        Forbid_Type_Coercion (CD, "left operand's type is integer, right operand's is floating-point");
        X.TYP := Floats;
        Emit1 (CD, k_Integer_to_Float, 1);
      elsif Y.TYP = Ints and X.TYP = Floats then
        Forbid_Type_Coercion (CD, "left operand's type is floating-point, right operand's is integer");
        Y.TYP := Floats;
        Emit1 (CD, k_Integer_to_Float, 0);
      elsif X.TYP = Enums and Y.TYP = Enums and X.Ref /= Y.Ref then
        Error (CD, err_incompatible_types_for_comparison);
      elsif X.TYP = Y.TYP then
        if PCode_Atomic_Typ (X.TYP) then
          Emit_Comparison_Instruction (CD, OP, X.TYP);
        else
          Error (CD, err_operator_not_defined_for_types);
        end if;
      else
        Error (CD, err_incompatible_types_for_comparison);
      end if;
      X.TYP := Bools;  --  The result of the comparison is always Boolean.
    end if;
    --
    if X.TYP = NOTYP and then CD.Err_Count = 0 then
      raise Internal_error with "Typeless expression, but no compilation error";
    end if;
  end Expression;

  procedure Boolean_Expression (
    CD    : in out Compiler_Data;
    Level :        Integer;
    FSys  :        Symset;
    X     :    out Exact_Typ
  )
  is
  begin
    Expression (CD, Level, FSys, X);
    Check_Boolean (CD, X.TYP);
  end Boolean_Expression;

end HAC.Parser.Expressions;
