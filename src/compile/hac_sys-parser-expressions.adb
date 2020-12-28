with HAC_Sys.Compiler.PCode_Emit;
with HAC_Sys.Parser.Attributes;
with HAC_Sys.Parser.Calls;                  use HAC_Sys.Parser.Calls;
with HAC_Sys.Parser.Helpers;                use HAC_Sys.Parser.Helpers;
with HAC_Sys.Parser.Standard_Functions;
with HAC_Sys.Parser.Type_Conversion;
with HAC_Sys.PCode;                         use HAC_Sys.PCode;
with HAC_Sys.Scanner;                       use HAC_Sys.Scanner;
with HAC_Sys.UErrors;                       use HAC_Sys.UErrors;
with HAC_Sys.Compiler;

package body HAC_Sys.Parser.Expressions is

  use Compiler.PCode_Emit;

  ------------------------------------------------------------------
  ---------------------------------------------------------Selector-
  procedure Selector (
    CD    : in out Compiler_Data;
    Level :        PCode.Nesting_level;
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
          Error (CD, err_undefined_identifier, stop => True);
        end if;
        V            := CD.IdTab (Field_Id).xTyp;
        Field_Offset := CD.IdTab (Field_Id).Adr_or_Sz;
        if Field_Offset /= 0 then
          Emit_1 (CD, k_Record_Field_Offset, Operand_2_Type (Field_Offset));
        end if;
      else
        Error (CD, err_var_with_field_selector_must_be_record);
      end if;
      InSymbol (CD);
    end Record_Field_Selector;
    --
    procedure Array_Coordinates_Selector is
      Array_Index_Expr : Exact_Typ;  --  Evaluation of "i", "j+7", "k*2" in "a (i, j+7, k*2)".
    begin
      loop
        InSymbol (CD);  --  Consume '(' or ',' symbol.
        Expression (CD, Level, FSys + Comma_RParent + RBrack, Array_Index_Expr);
        if V.TYP = Arrays then
          declare
            ATI : constant Integer := V.Ref;
            ATE : ATabEntry renames CD.Arrays_Table (ATI);
          begin
            if ATE.Index_xTyp /= Array_Index_Expr then
              Type_Mismatch (
                CD, err_illegal_array_subscript,
                Found    => Array_Index_Expr,
                Expected => ATE.Index_xTyp
              );
            elsif ATE.Element_Size = 1 then
              Emit_1 (CD, k_Array_Index_Element_Size_1, Operand_2_Type (ATI));
            else
              Emit_1 (CD, k_Array_Index, Operand_2_Type (ATI));
            end if;
            V := ATE.Element_xTyp;
          end;
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

  logical_operator : constant Symset :=             --  RM 4.5 (2)
    (AND_Symbol | OR_Symbol | XOR_Symbol => True,
     others => False);

  relational_operator : constant Symset :=          --  RM 4.5 (2)
    (Comparison_Operator => True, others => False);

  binary_adding_operator : constant Symset :=       --  RM 4.5 (4)
    (Plus | Minus | Ampersand_Symbol => True,
     others => False);

  multiplying_operator : constant Symset :=         --  RM 4.5 (6)
    (Times | Divide | MOD_Symbol | REM_Symbol => True,
     others => False);

  highest_precedence_operator : constant Symset :=  --  RM 4.5 (7)
    (ABS_Symbol | NOT_Symbol | Power => True,
     others => False);

  ------------------------------------------------------------------
  -------------------------------------------------------Expression-
  procedure Expression (
    CD    : in out Compiler_Data;
    Level :        PCode.Nesting_level;
    FSys  :        Symset;
    X     :    out Exact_Typ
  )
  is
    procedure Issue_Undefined_Operator_Error (Undef_OP : KeyWSymbol; X, Y : Exact_Typ) is
    begin
      Operator_Undefined (CD, Undef_OP, X, Y);
    end Issue_Undefined_Operator_Error;

    procedure Relation (FSys_Rel : Symset; X : out Exact_Typ) is            --  RM 4.4 (3)

      procedure Simple_Expression (FSys_SE : Symset; X : out Exact_Typ) is  --  RM 4.4 (4)

        procedure Term (FSys_Term : Symset; X : out Exact_Typ) is           --  RM 4.4 (5)

          procedure Factor (FSys_Fact : Symset; X : out Exact_Typ) is       --  RM 4.4 (6)

            procedure Primary (FSys_Prim : Symset; X : out Exact_Typ) is    --  RM 4.4 (7)
              F   : Opcode;
              err : Compile_Error;
              Ident_Index : Integer;
            begin  --  Factor
              X := Type_Undefined;
              Test (CD, Factor_Begin_Symbol + StrCon, FSys_Prim, err_factor_unexpected_symbol);
              if CD.Sy = StrCon then
                X.TYP := String_Literals;
                Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (CD.SLeng));  --  String Literal Length
                Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (CD.INum));   --  Index To String IdTab
                InSymbol (CD);
              end if;
              while Factor_Begin_Symbol (CD.Sy) loop  --  !!  Why a loop here ?... Why StrCon excluded ?
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
                          X := r.xTyp;
                          if X.TYP = Floats then
                            --  Address is an index in the float constants table.
                            Emit_1 (CD, k_Push_Float_Literal, Operand_2_Type (r.Adr_or_Sz));
                          else
                            --  Here the address is actually the immediate (discrete) value.
                            Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (r.Adr_or_Sz));
                          end if;
                          --
                        when Variable =>
                          X := r.xTyp;
                          if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
                            if r.Normal then
                              F := k_Push_Address;  --  Composite: push "v'Access".
                            else
                              F := k_Push_Value;    --  Composite: push "(v.all)'Access, that is, v.
                            end if;
                            Emit_2 (CD, F, r.LEV, Operand_2_Type (r.Adr_or_Sz));
                            Selector (CD, Level, FSys_Prim, X);
                            if Standard_or_Enum_Typ (X.TYP) then
                              --  We are at a leaf point of composite type selection,
                              --  so the stack top is expected to contain a value, not
                              --  an address (for an expression).
                              Emit (CD, k_Dereference);
                            end if;
                          else
                            --  No selector.
                            if Standard_or_Enum_Typ (X.TYP) then
                              if r.Normal then
                                F := k_Push_Value;           --  Push variable v's value.
                              else
                                F := k_Push_Indirect_Value;  --  Push "v.all" (v is an access).
                              end if;
                            elsif r.Normal then
                              F := k_Push_Address;  --  Composite: push "v'Access".
                            else
                              F := k_Push_Value;    --  Composite: push "(v.all)'Access, that is, v.
                            end if;
                            Emit_2 (CD, F, r.LEV, Operand_2_Type (r.Adr_or_Sz));
                          end if;
                          --
                        when TypeMark =>
                          Subtype_Prefixed_Expression (CD, Level, FSys_Prim, X);
                          --
                        when Prozedure =>
                          Error (CD, err_expected_constant_function_variable_or_subtype);
                          --
                        when Funktion =>
                          X := r.xTyp;
                          if r.LEV = 0 then
                            Standard_Functions.Standard_Function
                              (CD, Level, FSys_Prim, Ident_Index, SF_Code'Val (r.Adr_or_Sz), X);
                          else
                            Subprogram_or_Entry_Call (CD, Level, FSys_Prim, Ident_Index, Normal_Procedure_Call);
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
                        Emit_1 (CD, k_Push_Float_Literal, Operand_2_Type (RNum_Index));
                      end;
                    else
                      if CD.Sy = CharCon then
                        X.TYP := Chars;
                      else
                        X.TYP := Ints;
                      end if;
                      Emit_1 (CD, k_Push_Discrete_Literal, CD.INum);
                    end if;
                    X.Ref := 0;
                    InSymbol (CD);
                    --
                  when LParent =>    --  (
                    InSymbol (CD);
                    Expression (CD, Level, FSys_Prim + RParent, X);
                    Need (CD, RParent, err_closing_parenthesis_missing);
                    --
                  when others =>
                    null;
                end case;
                --
                if FSys_Prim = Semicolon_Set then
                  err := err_semicolon_missing;
                else
                  err := err_incorrectly_used_symbol;
                end if;
                Test (CD, FSys_Prim, Factor_Begin_Symbol, err);
              end loop;
            end Primary;

            Y : Exact_Typ;

          begin  --  Factor
            case CD.Sy is
              when ABS_Symbol =>
                InSymbol (CD);
                Primary (FSys_Fact, X);
                if X.TYP = Ints then
                  Emit_Std_Funct (CD, SF_Abs_Int);
                elsif X.TYP = Floats then
                  Emit_Std_Funct (CD, SF_Abs_Float);
                elsif X.TYP /= NOTYP then
                  Error (CD, err_argument_to_std_function_of_wrong_type);
                end if;
              when NOT_Symbol =>
                InSymbol (CD);
                Primary (FSys_Fact, X);
                if X.TYP = Bools then
                  Emit (CD, k_NOT_Boolean);
                elsif X.TYP /= NOTYP then
                  Error (CD, err_resulting_type_should_be_Boolean);
                end if;
              when others =>
                Primary (FSys_Fact + highest_precedence_operator, X);
                if CD.Sy = Power then
                  InSymbol (CD);
                  Primary (FSys_Fact, Y);
                  if X.TYP in Numeric_Typ and then X.TYP = Y.TYP then
                    Emit_Arithmetic_Binary_Instruction (CD, Power, X.TYP);
                  elsif X.TYP = Floats and Y.TYP = Ints then
                    Emit (CD, k_Power_Float_Integer);
                  else
                    Error (CD, err_invalid_power_operands);
                  end if;
                end if;
            end case;
          end Factor;

          Mult_OP : KeyWSymbol;
          Y       : Exact_Typ;
        begin  --  Term
          Factor (FSys_Term + multiplying_operator, X);
          --
          --  We collect here eventual factors: a {* b}
          --
          while multiplying_operator (CD.Sy) loop
            Mult_OP := CD.Sy;
            InSymbol (CD);
            Factor (FSys_Term + multiplying_operator, Y);
            if X.TYP = NOTYP or Y.TYP = NOTYP then
              null;  --  Something is already wrong at this point; nothing to check or emit.
            else
              case Mult_OP is
                when Times =>     --  *
                  if X.TYP in Numeric_Typ and then Y.TYP in Numeric_Typ then
                    if X.TYP = Y.TYP then
                      Emit_Arithmetic_Binary_Instruction (CD, Mult_OP, X.TYP);
                    else
                      Forbid_Type_Coercion (CD, Mult_OP, X, Y);
                    end if;
                  elsif X.TYP = Ints then
                    --  N * (something non-numeric)
                    case Y.TYP is
                      when Chars =>
                        Emit_Std_Funct (CD, SF_Int_Times_Char);  --  N * Some_Char
                        X.TYP := VStrings;
                      when String_Literals =>
                        --  Y is on top of the stack, we turn it into a VString.
                        Emit_Std_Funct (CD, SF_Literal_to_VString);
                        Emit_Std_Funct (CD, SF_Int_Times_VStr);  --  N * Some_String_Literal
                        X.TYP := VStrings;
                      when VStrings =>
                        Emit_Std_Funct (CD, SF_Int_Times_VStr);  --  N * Some_VString
                        X.TYP := VStrings;
                      when others =>
                        Issue_Undefined_Operator_Error (Mult_OP, X, Y);
                    end case;
                  else
                    Issue_Undefined_Operator_Error (Mult_OP, X, Y);
                  end if;
                when Divide =>    --  /
                  if X.TYP in Numeric_Typ and then X.TYP = Y.TYP then
                    Emit_Arithmetic_Binary_Instruction (CD, Mult_OP, X.TYP);
                  else
                    if X.TYP = Ints then
                      Forbid_Type_Coercion (CD, Mult_OP, X, Y);
                      Emit_1 (CD, k_Integer_to_Float, 1);  --  NB: this assumed Y.TYP was Floats!
                      X.TYP := Floats;
                    end if;
                    if Y.TYP = Ints then
                      Forbid_Type_Coercion (CD, Mult_OP, X, Y);
                      Emit_1 (CD, k_Integer_to_Float, 0);  --  NB: this assumed Y.TYP was Floats!
                      Y.TYP := Floats;
                    end if;
                    Error (CD, err_illegal_type_for_arithmetic_expression);
                    X.TYP := NOTYP;
                  end if;
                when MOD_Symbol | REM_Symbol =>
                  if X.TYP = Ints and Y.TYP = Ints then
                    if Mult_OP = MOD_Symbol then
                      Emit (CD, k_MOD_Integer);
                    else
                      Emit (CD, k_REM_Integer);
                    end if;
                  else
                    Error (CD, err_mod_requires_integer_arguments);
                    X.TYP := NOTYP;
                  end if;
                when others =>
                  raise Internal_error with "Unknown operator in Term";
              end case;
            end if;
          end loop;
        end Term;

        Adding_OP : KeyWSymbol;
        Y         : Exact_Typ;
      begin  --  Simple_Expression
        if Plus_Minus (CD.Sy) then
          --
          --  Unary + , -      RM 4.5 (5), 4.4 (4)
          --
          Adding_OP := CD.Sy;
          InSymbol (CD);
          Term (FSys_SE + Plus_Minus, X);
          if Adding_OP = Plus and X.TYP = String_Literals then        --  +"Hello"
            Emit_Std_Funct (CD, SF_Literal_to_VString);
            X.TYP := VStrings;
          elsif Adding_OP = Plus and X.TYP = Chars then               --  +'H'
            Emit_Std_Funct (CD, SF_Char_to_VString);
            X.TYP := VStrings;
          elsif Adding_OP = Plus and then Is_Char_Array (CD, X) then  --  +S
            Emit_Std_Funct (CD,
              SF_String_to_VString,
              Operand_1_Type (CD.Arrays_Table (X.Ref).Array_Size)
            );
            X.TYP := VStrings;
          elsif X.TYP not in Numeric_Typ then
            Error (CD, err_illegal_type_for_arithmetic_expression);
          elsif Adding_OP = Minus then
            Emit_Unary_Minus (CD, X.TYP);
          end if;
        else
          Term (FSys_SE + binary_adding_operator, X);
        end if;
        --
        --  We collect here eventual terms: a {+ b}      RM 4.4 (4)
        --
        while binary_adding_operator (CD.Sy) loop
          Adding_OP := CD.Sy;
          InSymbol (CD);
          Term (FSys_SE + binary_adding_operator, Y);
          if X.TYP = NOTYP or Y.TYP = NOTYP then
            null;  --  Something is already wrong at this point; nothing to check or emit.
          else
            case Adding_OP is
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
                if X.TYP in Numeric_Typ and then Y.TYP in Numeric_Typ then
                  if X.TYP = Y.TYP then
                    Emit_Arithmetic_Binary_Instruction (CD, Adding_OP, X.TYP);
                  else
                    Forbid_Type_Coercion (CD, Adding_OP, X, Y);
                  end if;
                elsif X.TYP = Times and Y.TYP = Times and Adding_OP = Minus then
                  Emit_Std_Funct (CD, SF_Time_Subtract);  --  T2 - T1
                  X.TYP := Durations;
                elsif X.TYP = Durations then
                  if Y.TYP = Floats then
                    --  Duration hack for "X + 1.234" (see Delay_Statement
                    --  for full explanation).
                    Emit_Std_Funct (CD, SF_Float_to_Duration);
                    Y.TYP := Durations;  --  Now X and Y have the type Duration.
                  end if;
                  if Y.TYP = Durations then
                    if Adding_OP = Plus then
                      Emit_Std_Funct (CD, SF_Duration_Add);
                    else
                      Emit_Std_Funct (CD, SF_Duration_Subtract);
                    end if;
                  else
                    Issue_Undefined_Operator_Error (Adding_OP, X, Y);
                  end if;
                else
                  Issue_Undefined_Operator_Error (Adding_OP, X, Y);
                end if;
              when Ampersand_Symbol =>
                --  Concatenation. RM References: Unbounded_String.
                if X.TYP = VStrings and Y.TYP = VStrings then            --  v & v     RM A.4.5 (15)
                  Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
                elsif X.TYP = VStrings and Y.TYP = String_Literals then  --  v & "x"   RM A.4.5 (16)
                  --  Y is on top of the stack, we turn it into a VString.
                  --  If this becomes a perfomance issue we could consider
                  --  an opcode for (VStr op Lit_Str).
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
                elsif X.TYP = VStrings and Y.TYP = Ints then             --  v & 123
                  Emit_Std_Funct (CD, SF_VString_Int_Concat);
                elsif X.TYP = Ints and Y.TYP = VStrings then             --  123 & v
                  Emit_Std_Funct (CD, SF_Int_VString_Concat);
                  X.TYP := VStrings;
                elsif X.TYP = VStrings and Y.TYP = Floats then           --  v & 3.14159
                  Emit_Std_Funct (CD, SF_VString_Float_Concat);
                elsif X.TYP = Floats and Y.TYP = VStrings then           --  3.14159 & v
                  Emit_Std_Funct (CD, SF_Float_VString_Concat);
                  X.TYP := VStrings;
                else
                  Issue_Undefined_Operator_Error (Adding_OP, X, Y);
                end if;
              when others =>  --  Doesn't happen: Binary_Adding_Operators(OP) is True.
                null;
            end case;
          end if;
        end loop;
      end Simple_Expression;

      Rel_OP : KeyWSymbol;
      Y      : Exact_Typ;

      procedure Issue_Comparison_Type_Mismatch_Error is
      begin
        Type_Mismatch (CD, err_incompatible_types_for_comparison, Found => Y, Expected => X);
      end Issue_Comparison_Type_Mismatch_Error;

    begin  --  Relation
      Simple_Expression (FSys_Rel + relational_operator, X);
      --
      --  We collect here an eventual comparison: a {= b}
      --
      if relational_operator (CD.Sy) then
        Rel_OP := CD.Sy;
        InSymbol (CD);
        Simple_Expression (FSys_Rel, Y);
        if X.TYP = Ints and Y.TYP = Floats then
          Forbid_Type_Coercion (CD, Rel_OP, X, Y);
          X.TYP := Floats;
          Emit_1 (CD, k_Integer_to_Float, 1);
        elsif X.TYP = Floats and Y.TYP = Ints then
          Forbid_Type_Coercion (CD, Rel_OP, X, Y);
          Y.TYP := Floats;
          Emit_1 (CD, k_Integer_to_Float, 0);
        elsif X.TYP = Enums and Y.TYP = Enums and X.Ref /= Y.Ref then
          Issue_Comparison_Type_Mismatch_Error;
        elsif X.TYP = VStrings and Y.TYP = String_Literals then  --  V = "Hello", V < "World", etc.
          --  Y is on top of the stack, we turn it into a VString.
          --  If this becomes a perfomance issue we could consider an opcode for (VStr op Lit_Str).
          Emit_Std_Funct (CD, SF_Literal_to_VString);  --  Now we have e.g. V < +"World".
          Emit_Comparison_Instruction (CD, Rel_OP, VStrings);  --  Emit "<" (X, Y) between VStrings.
        elsif X.TYP = Y.TYP then
          if PCode_Atomic_Typ (X.TYP) then
            Emit_Comparison_Instruction (CD, Rel_OP, X.TYP);
          else
            Issue_Undefined_Operator_Error (Rel_OP, X, Y);
          end if;
        else
          Issue_Comparison_Type_Mismatch_Error;
        end if;
        X.TYP := Bools;  --  The result of the comparison is always Boolean.
      end if;
      --
      if X.TYP = NOTYP and then CD.Err_Count = 0 then
        raise Internal_error with "Typeless expression, but no compilation error";
      end if;
    end Relation;

    Logical_OP : KeyWSymbol;
    Y          : Exact_Typ;

  begin  --  Expression
    Relation (FSys + logical_operator, X);
    --
    --  RM 4.4 (2): we collect here eventual relations, connected by
    --  logical operators: x {and y}.
    --
    while logical_operator (CD.Sy) loop
      Logical_OP := CD.Sy;
      InSymbol (CD);
      Relation (FSys + logical_operator, Y);
      if X.TYP = Bools and Y.TYP = Bools then
        case Logical_OP is
          when AND_Symbol => Emit (CD, k_AND_Boolean);
          when OR_Symbol  => Emit (CD, k_OR_Boolean);
          when XOR_Symbol => Emit (CD, k_XOR_Boolean);
          when others     => null;
        end case;
      else
        Error (CD, err_resulting_type_should_be_Boolean);
        X.TYP := NOTYP;
      end if;
    end loop;
  end Expression;

  procedure Boolean_Expression (
    CD    : in out Compiler_Data;
    Level :        PCode.Nesting_level;
    FSys  :        Symset;
    X     :    out Exact_Typ
  )
  is
  begin
    Expression (CD, Level, FSys, X);
    Check_Boolean (CD, X.TYP);
  end Boolean_Expression;

  procedure Subtype_Prefixed_Expression (
    CD    : in out Compiler_Data;
    Level : in     PCode.Nesting_level;
    FSys  : in     Defs.Symset;
    X     :    out Exact_Typ
  )
  is
    Type_ID : constant String := To_String (CD.Id);
    Mem_Sy : constant KeyWSymbol := CD.Sy;
  begin
    InSymbol (CD);
    case Mem_Sy is
      when LParent    => Type_Conversion (CD, Level, FSys, Type_ID, X);
      when Apostrophe => Attributes.Scalar_Subtype_Attribute (CD, Level, FSys, Type_ID, X);
      when others => Error (CD, err_syntax_error, "expected ""'"" or ""("" here", True);
    end case;
  end Subtype_Prefixed_Expression;

end HAC_Sys.Parser.Expressions;
