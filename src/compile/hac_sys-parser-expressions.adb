with HAC_Sys.Compiler.PCode_Emit;
with HAC_Sys.Parser.Attributes;
with HAC_Sys.Parser.Calls;
with HAC_Sys.Parser.Helpers;
with HAC_Sys.Parser.Ranges;
with HAC_Sys.Parser.Standard_Functions;
with HAC_Sys.Parser.Type_Conversion;
with HAC_Sys.PCode;
with HAC_Sys.Scanner;
with HAC_Sys.Errors;

package body HAC_Sys.Parser.Expressions is

  use Compiler.PCode_Emit, Co_Defs, Defs, Helpers, PCode, Scanner, Errors;

  procedure Static_Scalar_Expression (
    CD      : in out Co_Defs.Compiler_Data;
    Level   : in     Defs.Nesting_level;
    FSys_ND : in     Defs.Symset;
    C       :    out Co_Defs.Constant_Rec
  )
  is
    --  This covers number declarations (RM 3.3.2) and enumeration items (RM 3.5.1).
    --  Additionally this compiler does on-the-fly declarations for static values:
    --  bounds in ranges (FOR, ARRAY), and values in CASE statements.
    --  Was: Constant in the Pascal compiler.
    X : Integer;
    Sign : HAC_Integer;
    use type HAC_Float, HAC_Integer;
    signed : Boolean := False;
    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;
  begin
    C.TP := Undefined;
    C.I  := 0;
    Test (CD, Constant_Definition_Begin_Symbol, FSys_ND, err_illegal_symbol_for_a_number_declaration);
    if not Constant_Definition_Begin_Symbol (CD.Sy) then
      return;
    end if;
    if CD.Sy = CharCon then  --  Untyped character constant, occurs only in ranges.
      Construct_Root (C.TP, Chars);
      C.I  := CD.INum;
      InSymbol;
    else
      Sign := 1;
      if Plus_Minus (CD.Sy) then
        signed := True;
        if CD.Sy = Minus then
          Sign := -1;
        end if;
        InSymbol;
      end if;
      case CD.Sy is
        when IDent =>
          --  Number defined using another one: "minus_pi : constant := -pi;"
          --  ... or, we have an enumeration item.
          X := Locate_Identifier (CD, CD.Id, Level);
          if X /= 0 then
            if CD.IdTab (X).entity = Declared_Number_or_Enum_Item then
              C.TP := CD.IdTab (X).xtyp;
              if C.TP.TYP = Floats then
                C.R := HAC_Float (Sign) * CD.Float_Constants_Table (CD.IdTab (X).adr_or_sz);
              else
                C.I := Sign * HAC_Integer (CD.IdTab (X).adr_or_sz);
                if signed and then C.TP.TYP not in Numeric_Typ then
                  Error (CD, err_numeric_constant_expected);
                end if;
              end if;
            else
              Error (CD, err_illegal_constant_or_constant_identifier, severity => major);
            end if;
          end if;  --  X /= 0
          InSymbol;
        when IntCon =>
          C.TP.Construct_Root (Ints);
          C.I  := Sign * CD.INum;
          InSymbol;
        when FloatCon =>
          C.TP.Construct_Root (Floats);
          C.R  := HAC_Float (Sign) * CD.RNum;
          InSymbol;
        when others =>
          Skip (CD, FSys_ND, err_illegal_symbol_for_a_number_declaration);
      end case;
    end if;
    Test (CD, FSys_ND, Empty_Symset, err_incorrectly_used_symbol);
  end Static_Scalar_Expression;

  ------------------------------------------------------------------
  ---------------------------------------------------------Selector-
  procedure Selector (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    V     : in out Co_Defs.Exact_Subtyp
  )
  is
    --
    procedure Record_Field_Selector is
      Field_Offset, Field_Id : Integer;
      use type Alfa;
    begin
      if V.TYP = Records then
        Field_Id := CD.Blocks_Table (V.Ref).Last_Id_Idx;
        CD.IdTab (0).name := CD.Id;
        while CD.IdTab (Field_Id).name /= CD.Id loop  --  Search field identifier
          Field_Id := CD.IdTab (Field_Id).link;
        end loop;
        if Field_Id = No_Id then
          Error (CD, err_undefined_identifier, A2S (CD.Id_with_case), major);
        end if;
        V            := CD.IdTab (Field_Id).xtyp;
        Field_Offset := CD.IdTab (Field_Id).adr_or_sz;
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
      Array_Index_Typ : Exact_Subtyp;  --  Evaluation of "i", "j+7", "k*2" in "a (i, j+7, k*2)".
      range_check_needed : Boolean;
      use type HAC_Integer;
    begin
      loop
        InSymbol (CD);  --  Consume '(' or ',' symbol.
        Expression (CD, Level, FSys + Comma_RParent + RBrack, Array_Index_Typ);
        if V.TYP = Arrays then
          declare
            ATI : constant Integer := V.Ref;
            ATE : ATabEntry renames CD.Arrays_Table (ATI);
          begin
            if Exact_Typ (ATE.Index_xTyp) /= Exact_Typ (Array_Index_Typ) then
              Type_Mismatch (
                CD, err_illegal_array_subscript,
                Found    => Array_Index_Typ,
                Expected => ATE.Index_xTyp
              );
            else
              if Ranges.Do_Ranges_Overlap (Array_Index_Typ,  ATE.Index_xTyp) then
                range_check_needed :=
                     Array_Index_Typ.Discrete_First < ATE.Index_xTyp.Discrete_First
                  or Array_Index_Typ.Discrete_Last  > ATE.Index_xTyp.Discrete_Last;
                --
                if ATE.Element_Size = 1 then
                  if range_check_needed then
                    Emit_1 (CD, k_Array_Index_Element_Size_1, Operand_2_Type (ATI));
                  else
                    Emit_1 (CD, k_Array_Index_No_Check_Element_Size_1, Operand_2_Type (ATI));
                  end if;
                else
                  if range_check_needed then
                    Emit_1 (CD, k_Array_Index, Operand_2_Type (ATI));
                  else
                    Emit_1 (CD, k_Array_Index_No_Check, Operand_2_Type (ATI));
                  end if;
                end if;
              else
                Error
                  (CD, err_range_constraint_error,
                   "value of index (" &
                   (if Ranges.Is_Singleton_Range (Array_Index_Typ) then
                      --  More understandable message part for a single value
                      Discrete_Image
                       (CD,
                        Array_Index_Typ.Discrete_First,
                        ATE.Index_xTyp.TYP,
                        ATE.Index_xTyp.Ref)
                    else
                      "range: " &
                      Discrete_Range_Image
                        (CD,
                         Array_Index_Typ.Discrete_First,
                         Array_Index_Typ.Discrete_Last,
                         ATE.Index_xTyp.TYP,
                         ATE.Index_xTyp.Ref)) &
                      ") is out of the array's range, " &
                      Discrete_Range_Image
                        (CD,
                         ATE.Index_xTyp.Discrete_First,
                         ATE.Index_xTyp.Discrete_Last,
                         ATE.Index_xTyp.TYP,
                         ATE.Index_xTyp.Ref),
                      minor);
              end if;
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
    Test
      (CD, FSys, Empty_Symset,
       (if FSys = Semicolon_Set then err_semicolon_missing else err_incorrectly_used_symbol));
  end Selector;

  logical_operator : constant Symset :=             --  RM 4.5 (2)
    (AND_Symbol | OR_Symbol | XOR_Symbol => True,
     others => False);

  relational_operator : constant Symset :=          --  RM 4.5 (3)
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

  procedure Issue_Undefined_Operator_Error (
    CD       : in out Co_Defs.Compiler_Data;
    Undef_OP :        KeyWSymbol;
    X, Y     :        Exact_Subtyp)
  is
  begin
    Operator_Undefined (CD, Undef_OP, X, Y);
  end Issue_Undefined_Operator_Error;

  Internally_VString_Set : constant Typ_Set := VStrings_Set or Str_as_VStr_Set;

  ------------------------------------------------------------------
  -------------------------------------------------------Expression-
  procedure Expression (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    X     :    out Co_Defs.Exact_Subtyp
  )
  is
    procedure Relation (FSys_Rel : Symset; X : out Exact_Subtyp) is  --  RM 4.4 (3)
      Y : Exact_Subtyp;

      procedure Issue_Comparison_Type_Mismatch_Error is
      begin
        Type_Mismatch (CD, err_incompatible_types_for_comparison, Found => Y, Expected => X);
      end Issue_Comparison_Type_Mismatch_Error;

      Rel_OP : KeyWSymbol;
      Not_In : Boolean;

    begin  --  Relation
      --
      --  Single  simple_expression,  or:  simple_expression OPERATOR simple_expression
      --
      Simple_Expression (CD, Level, FSys_Rel + relational_operator + IN_Symbol + NOT_Symbol, X);
      --
      case CD.Sy is
        when Comparison_Operator =>
          --
          --  We collect here a comparison (relational) operator, e.g.: x < y
          --
          Rel_OP := CD.Sy;
          InSymbol (CD);
          Simple_Expression (CD, Level, FSys_Rel, Y);
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
          elsif Internally_VString_Set (X.TYP) and Y.TYP = String_Literals then  --  E.g., X < "World"
            --  Y is on top of the stack, we turn it into a VString.
            --  If this becomes a perfomance issue we could consider
            --  a new Standard Function (SF_Code) for (VStr op Lit_Str).
            Emit_Std_Funct (CD, SF_Literal_to_VString);            --  Now we have X < +"World".
            Emit_Comparison_Instruction (CD, Rel_OP, VStrings);    --  Emit "<" (X, +Y).
          elsif Internally_VString_Set (X.TYP) and Internally_VString_Set (Y.TYP) then
            --  The internal type is actually a VString on both sides.
            Emit_Comparison_Instruction (CD, Rel_OP, VStrings);
          elsif Is_Char_Array (CD, X) and Y.TYP = String_Literals then
            --  We needs convert the literal before anything else,
            --  since it takes two elements on the stack.
            Emit_Std_Funct (CD, SF_Literal_to_VString);
            Emit (CD, k_Swap);
            Emit_Std_Funct (CD,
              SF_String_to_VString,
              Operand_1_Type (CD.Arrays_Table (X.Ref).Array_Size)
            );
            Emit (CD, k_Swap);
            Emit_Comparison_Instruction (CD, Rel_OP, VStrings);
          elsif X.TYP = Y.TYP then
            if PCode_Atomic_Typ (X.TYP) then
              Emit_Comparison_Instruction (CD, Rel_OP, X.TYP);
            else
              Issue_Undefined_Operator_Error (CD, Rel_OP, X, Y);
            end if;
          else
            Issue_Comparison_Type_Mismatch_Error;
          end if;
          Construct_Root (X, Bools);  --  The result of the comparison is always Boolean.
        when IN_Symbol | NOT_Symbol =>
          --
          --  We collect here a membership test, e.g.: x [not] in a .. b
          --
          Not_In := CD.Sy = NOT_Symbol;
          InSymbol (CD);
          if Not_In then
            Need (CD, IN_Symbol, err_IN_missing);
          end if;
          if CD.error_count = 0 then
            Ranges.Dynamic_Range (CD, Level, FSys_Rel, err_discrete_type_expected, Y);
            if Exact_Typ (X) /= Exact_Typ (Y) then
              Type_Mismatch (CD, err_membership_test_type_mismatch, Found => Y, Expected => X);
              --  The RM 4.5.2 (2) seems to accept any types for X and Y. The test would be False
              --  if types were incompatible. However, in that situation, GNAT says
              --  "incompatible types", ObjectAda says "LRM:8.6(28), Inappropriate operands
              --  for "IN" operation".
            end if;
            if Not_In then
              Emit_Std_Funct (CD, SF_not_in_discrete_Interval);
            else
              Emit_Std_Funct (CD, SF_in_discrete_Interval);
            end if;
            Construct_Root (X, Bools);  --  The result of the membership test is always Boolean.
          end if;
        when others =>
          null;
      end case;
    end Relation;

    Logical_OP    : KeyWSymbol;
    Y             : Exact_Subtyp;
    short_circuit : Boolean;
    LC_Cond_Jump  : Integer;

    procedure Process_Short_Circuit (Cond_Jump : Opcode) is
    begin
      InSymbol (CD);
      short_circuit := True;
      LC_Cond_Jump := CD.LC;
      Emit (CD, Cond_Jump);
      Emit (CD, k_Pop);      --  Discard X value from stack. Top item will be Y.
    end Process_Short_Circuit;

  begin  --  Expression
    Relation (FSys + logical_operator, X);
    --
    --  RM 4.4 (2): we collect here eventual relations, connected by
    --              logical operators: X {and Y}.
    --
    while logical_operator (CD.Sy) loop
      Logical_OP := CD.Sy;
      InSymbol (CD);
      --
      --  Short-circuit forms of AND, OR.
      --
      short_circuit := False;
      if Logical_OP = AND_Symbol and CD.Sy = THEN_Symbol then
        Process_Short_Circuit (k_Jump_If_Zero_No_Pop);
        --
        --    Jump on X = False (i.e. 0). If X = True, then X and Y = Y.
        --
        --       X          :    0      0      1      1
        --                         \      \
        --       Y          :    0  |   1  |   0      1
        --                         /      /    |      |
        --       X and Y    :    0      0      0      1
        --
      elsif Logical_OP = OR_Symbol and CD.Sy = ELSE_Symbol then
        Process_Short_Circuit (k_Jump_If_Non_Zero_No_Pop);
        --
        --    Jump on X = True (i.e. 1). If X = False, then X or Y = Y.
        --
        --       X          :    0      0      1      1
        --                                       \      \
        --       Y          :    0      1      0  |   1  |
        --                       |      |        /      /
        --       X or Y     :    0      1      1      1
        --
      end if;
      --
      --  Right side of the logical operator.
      --
      Relation (FSys + logical_operator, Y);
      --
      if X.TYP = Bools and Y.TYP = Bools then
        if short_circuit then
          --  Patch the address for the conditional jump, with the place
          --  right after the evaluation of relation Y:
          CD.ObjCode (LC_Cond_Jump).Y := Operand_2_Type (CD.LC);
        else
          case Logical_OP is
            when AND_Symbol => Emit (CD, k_AND_Boolean);
            when OR_Symbol  => Emit (CD, k_OR_Boolean);
            when XOR_Symbol => Emit (CD, k_XOR_Boolean);
            when others     => null;
          end case;
        end if;
      else
        Error (CD, err_resulting_type_should_be_Boolean);
        X.TYP := NOTYP;
      end if;
    end loop;
    if X.TYP = NOTYP and then CD.error_count = 0 then
      Error (CD, err_object_used_before_end_own_declaration, severity => major);
    end if;
  end Expression;

  procedure Simple_Expression (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    X     :    out Co_Defs.Exact_Subtyp
  )
  is  --  RM 4.4 (4)

    procedure Term (FSys_Term : Symset; X : out Exact_Subtyp) is         --  RM 4.4 (5)

      procedure Factor (FSys_Fact : Symset; X : out Exact_Subtyp) is     --  RM 4.4 (6)

        procedure Primary (FSys_Prim : Symset; X : out Exact_Subtyp) is  --  RM 4.4 (7)
          LC_Mem : Integer;
        begin
          X := Undefined;
          Test (CD, Primary_Begin_Symbol + StrCon, FSys_Prim, err_primary_unexpected_symbol);
          case CD.Sy is
            when StrCon =>
              Construct_Root (X, String_Literals);
              Emit_2
                (CD, k_Push_Two_Discrete_Literals,
                 Operand_1_Type (CD.SLeng),  --  String Literal Length
                 Operand_2_Type (CD.INum));  --  Index To String IdTab
              InSymbol (CD);
            when IDent =>
              declare
                Ident_Index : constant Integer := Locate_Identifier (CD, CD.Id, Level);
                r : IdTabEntry renames CD.IdTab (Ident_Index);
              begin
                InSymbol (CD);
                case r.entity is
                  when Declared_Number_or_Enum_Item =>
                    X := r.xtyp;
                    if X.TYP = Floats then
                      --  Address is an index in the float constants table.
                      Emit_1 (CD, k_Push_Float_Literal, Operand_2_Type (r.adr_or_sz));
                    else
                      --  Here the address is actually the immediate (discrete) value.
                      Emit_1 (CD, k_Push_Discrete_Literal, Operand_2_Type (r.adr_or_sz));
                      --  The local subtype for the value V is the range V .. V.
                      Ranges.Set_Singleton_Range (X, HAC_Integer (r.adr_or_sz));
                    end if;
                    --
                  when Variable =>
                    X := r.xtyp;
                    LC_Mem := CD.LC;
                    if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
                      Emit_2
                        (CD,
                         (if r.normal then
                            k_Push_Address           --  Composite: push "v'Access".
                          else
                            k_Push_Discrete_Value),  --  Composite: push "(a.all)'Access", that is, a.
                         Operand_1_Type (r.lev),
                         Operand_2_Type (r.adr_or_sz));
                      Selector (CD, Level, FSys_Prim + Apostrophe, X);
                      if Standard_or_Enum_Typ (X.TYP) then
                        --  We are at a leaf point of composite type selection,
                        --  so the stack top is expected to contain a value, not
                        --  an address (for an expression).
                        Emit (CD, k_Dereference);
                      end if;
                    else
                      --  No selector.
                      Emit_2
                        (CD,
                         (if Standard_or_Enum_Typ (X.TYP) then
                           (if r.normal then
                             (if Discrete_Typ (r.xtyp.TYP) then
                                k_Push_Discrete_Value   --  Push variable v's discrete value.
                              else
                                k_Push_Value)           --  Push variable v's value.
                            else
                              k_Push_Indirect_Value)    --  Push "a.all" (a is an access).
                          elsif r.normal then
                            k_Push_Address              --  Composite: push "v'Access".
                          else
                            k_Push_Discrete_Value),     --  Composite: push "(a.all)'Access, that is, a.
                         Operand_1_Type (r.lev),
                         Operand_2_Type (r.adr_or_sz));
                    end if;
                    if CD.Sy = Apostrophe then
                      InSymbol (CD);
                      Attributes.Object_Attribute (CD, Level, FSys_Prim, X, LC_Mem, X);
                    end if;
                    --
                  when TypeMark =>
                    X := r.xtyp;
                    Subtype_Prefixed_Expression (CD, Level, FSys_Prim, Ident_Index, X);
                  when Prozedure | Prozedure_Intrinsic =>
                    Error (CD, err_expected_constant_function_variable_or_subtype);
                  when Funktion =>
                    X := r.xtyp;
                    Calls.Subprogram_or_Entry_Call
                      (CD, Level, FSys_Prim, Ident_Index, Normal_Procedure_Call);
                  when Funktion_Intrinsic =>
                    Standard_Functions.Standard_Function
                      (CD, Level, FSys_Prim, Ident_Index, SF_Code'Val (r.adr_or_sz), X);
                  when others =>
                    null;
                end case;
                if X.TYP = NOTYP and then CD.error_count = 0 then
                  Error
                    (CD, err_object_used_before_end_own_declaration,
                     '"' & A2S (r.name_with_case) & """ ", major);
                end if;
              end;
              --
            when CharCon | IntCon | FloatCon =>
              if CD.Sy = FloatCon then
                X.Construct_Root (Floats);
                Emit_Push_Float_Literal (CD, CD.RNum);
              else
                --  Here we have a discrete literal
                X.Construct_Root (if CD.Sy = CharCon then Chars else Ints);
                Emit_1 (CD, k_Push_Discrete_Literal, CD.INum);
                --  The local subtype for the value V is the range V .. V.
                Ranges.Set_Singleton_Range (X, CD.INum);
              end if;
              InSymbol (CD);
              --
            when LParent =>
              --  '(' : what is inside the parentheses is an
              --        expression of the lowest level.
              InSymbol (CD);
              Expression (CD, Level, FSys_Prim + RParent, X);
              Need (CD, RParent, err_closing_parenthesis_missing);
              --
            when others =>
              null;
          end case;
          if X.TYP = NOTYP and then CD.error_count = 0 then
            Error (CD, err_object_used_before_end_own_declaration, severity => major);
          end if;
        end Primary;

        Y : Exact_Subtyp;

      begin  --  Factor
        case CD.Sy is
          when ABS_Symbol =>
            InSymbol (CD);
            Primary (FSys_Fact, X);
            case X.TYP is
              when Ints   => Emit_Std_Funct (CD, SF_Abs_Int);
              when Floats => Emit_Std_Funct (CD, SF_Abs_Float);
              when NOTYP  => null;  --  Another error before.
              when others => Error (CD, err_argument_to_std_function_of_wrong_type);
            end case;
            X.Construct_Root (X.TYP);  --  Forget subtype bounds
          when NOT_Symbol =>
            InSymbol (CD);
            Primary (FSys_Fact, X);
            case X.TYP is
              when Bools => Emit (CD, k_NOT_Boolean);
              when NOTYP  => null;  --  Another error before.
              when others => Error (CD, err_resulting_type_should_be_Boolean);
            end case;
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
              X.Construct_Root (X.TYP);  --  Forget subtype bounds
            end if;
        end case;
      end Factor;

      use type HAC_Integer;
      Mult_OP : KeyWSymbol;
      Y       : Exact_Subtyp;
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
                --  Figure out the subtype range after the
                --  multiplication, if possible. NB: beyond trivial cases,
                --  compile-time overflow checks would be needed.
                if X.TYP = Ints then
                  --  Find some possible static values.
                  if Ranges.Is_Singleton_Range (X, 0) then
                    --  0 * Y = 0.
                    null;  --  Keep X's range, which is [0; 0].
                  elsif Ranges.Is_Singleton_Range (Y, 0) then
                    --  X * 0 = 0.
                    Ranges.Set_Singleton_Range (X, 0);
                  elsif Ranges.Is_Singleton_Range (X, 1) then
                    --  1 * Y = Y.
                    X.Discrete_First := Y.Discrete_First;
                    X.Discrete_Last  := Y.Discrete_Last;
                  elsif Ranges.Is_Singleton_Range (Y, 1) then
                    --  X * 1 = X.
                    null;  --  Keep X's range.
                  else
                    X.Construct_Root (X.TYP);  --  Forget subtype bounds
                  end if;
                end if;
              elsif X.TYP = Ints then
                --  N * (something non-numeric)
                case Y.TYP is
                  when Chars =>
                    Emit_Std_Funct (CD, SF_Int_Times_Char);  --  N * Some_Char
                    Construct_Root (X, VStrings);
                  when String_Literals =>
                    --  Y is on top of the stack, we turn it into a VString.
                    Emit_Std_Funct (CD, SF_Literal_to_VString);
                    Emit_Std_Funct (CD, SF_Int_Times_VStr);  --  N * Some_String_Literal
                    Construct_Root (X, VStrings);
                  when VStrings | Strings_as_VStrings =>
                    Emit_Std_Funct (CD, SF_Int_Times_VStr);  --  N * Some_VString
                    Construct_Root (X, VStrings);
                  when others =>
                    Issue_Undefined_Operator_Error (CD, Mult_OP, X, Y);
                end case;
              else
                Issue_Undefined_Operator_Error (CD, Mult_OP, X, Y);
              end if;
            when Divide =>    --  /
              if X.TYP in Numeric_Typ and then X.TYP = Y.TYP then
                Emit_Arithmetic_Binary_Instruction (CD, Mult_OP, X.TYP);
                X.Construct_Root (X.TYP);  --  Forget subtype bounds
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
    y         : Exact_Subtyp;

    function VString_Concatenation return Boolean is
    begin
      --  !!  Check if HAT is "use"-visible  !!
      --
      --  RM References are about Unbounded_String (A.4.5).
      if X.TYP /= VStrings and y.TYP /= VStrings then
        return False;
        --  Below this line, at least X or Y is a VString.
      elsif Internally_VString_Set (X.TYP) and Internally_VString_Set (y.TYP) then
        --  v1 & v2              A.4.5 (15)
        --  v & Enum'Image (x)   A.4.5 (16),
        --  Enum'Image (x) & v   A.4.5 (17)
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif y.TYP = String_Literals then                          --  v & "x"  A.4.5 (16)
        --  Y is on top of the stack, we turn it into a VString.
        --  If this becomes a perfomance issue we could consider
        --  adding a Standard Function (SF_Code) for (VStr op Lit_Str).
        Emit_Std_Funct (CD, SF_Literal_to_VString);
        --  Now we concatenate both VStrings.
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif X.TYP = String_Literals then                          --  "x" & v  A.4.5 (17)
        Emit_Std_Funct (CD, SF_LStr_VString_Concat);
      elsif Is_Char_Array (CD, y) then                            --  v & s    A.4.5 (16)
        Emit_Std_Funct (CD,
          SF_String_to_VString,
          Operand_1_Type (CD.Arrays_Table (y.Ref).Array_Size)
        );
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif Is_Char_Array (CD, X) then                            --  s & v    A.4.5 (17)
        Emit (CD, k_Swap);   --  v, then s on the stack
        Emit_Std_Funct (CD,  --  s -> +s
          SF_String_to_VString,
          Operand_1_Type (CD.Arrays_Table (X.Ref).Array_Size)
        );
        Emit (CD, k_Swap);   --  +s, then v on the stack
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif y.TYP = Chars then                                    --  v & 'x'  A.4.5 (18)
        Emit_Std_Funct (CD, SF_VString_Char_Concat);
      elsif X.TYP = Chars then                                    --  'x' & v  A.4.5 (19)
        Emit_Std_Funct (CD, SF_Char_VString_Concat);
      --
      --  Hereafter, we have "&" operators on VString provided only by HAT
      --  and not by Ada.Unbounded_Strings
      --
      elsif y.TYP = Ints then      Emit_Std_Funct (CD, SF_VString_Int_Concat);       --  v & 123
      elsif X.TYP = Ints then      Emit_Std_Funct (CD, SF_Int_VString_Concat);       --  123 & v
      elsif y.TYP = Floats then    Emit_Std_Funct (CD, SF_VString_Float_Concat);     --  v & 3.14159
      elsif X.TYP = Floats then    Emit_Std_Funct (CD, SF_Float_VString_Concat);     --  3.14159 & v
      elsif y.TYP = Durations then Emit_Std_Funct (CD, SF_VString_Duration_Concat);  --  v & dur
      elsif X.TYP = Durations then Emit_Std_Funct (CD, SF_Duration_VString_Concat);  --  dur & v
      elsif y.TYP = Bools then     Emit_Std_Funct (CD, SF_VString_Boolean_Concat);   --  v & is_found
      elsif X.TYP = Bools then     Emit_Std_Funct (CD, SF_Boolean_VString_Concat);   --  is_found & v
      else
        return False;
      end if;
      Construct_Root (X, VStrings);
      return True;
    end VString_Concatenation;

    function String_Concatenation return Boolean is
    begin
      --  Arguments can be one of the three internal representations of String:
      --      - sv : VString (the parser sees the TYP Strings_as_VStrings)
      --      - sc : constrained array of character
      --      - "x": literal string
      --  Additionally, we can have a character:
      --      - 'x': character (value or literal)
      --  So, it makes 16 argument combinations. Not all are implemented.
      --
      --  Result is always Strings_as_VStrings.
      --  RM Reference: the predefined "&" operator 4.5.3(3), applied to String.
      --
      if X.TYP = Strings_as_VStrings and y.TYP = Strings_as_VStrings then     --  sv1 & sv2
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif X.TYP = Strings_as_VStrings and then Is_Char_Array (CD, y) then   --  sv1 & sc2
        Emit_Std_Funct (CD,
          SF_String_to_VString,
          Operand_1_Type (CD.Arrays_Table (y.Ref).Array_Size)
        );
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif Is_Char_Array (CD, X) and then y.TYP = Strings_as_VStrings then   --  sc1 & sv2
        Emit (CD, k_Swap);   --  sc2, then sv1 on the stack
        Emit_Std_Funct (CD,  --  sc1 -> To_VString (sc1)
          SF_String_to_VString,
          Operand_1_Type (CD.Arrays_Table (X.Ref).Array_Size)
        );
        Emit (CD, k_Swap);   --  To_VString (sc1), then sv2 on the stack
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif X.TYP = Strings_as_VStrings and y.TYP = String_Literals then      --  sv & "x"
        Emit_Std_Funct (CD, SF_Literal_to_VString);
        Emit_Std_Funct (CD, SF_Two_VStrings_Concat);
      elsif X.TYP = String_Literals and y.TYP = Strings_as_VStrings then      --  "x" & sv
        Emit_Std_Funct (CD, SF_LStr_VString_Concat);
      elsif X.TYP = Strings_as_VStrings and y.TYP = Chars then                --  sv & 'x'
        Emit_Std_Funct (CD, SF_VString_Char_Concat);
      elsif X.TYP = Chars and y.TYP = Strings_as_VStrings then                --  'x' & sv
        Emit_Std_Funct (CD, SF_Char_VString_Concat);
      else
        return False;
      end if;
      Construct_Root (X, Strings_as_VStrings);
      return True;
    end String_Concatenation;

  begin  --  Simple_Expression
    if Plus_Minus (CD.Sy) then
      --
      --  Unary + , -      RM 4.5 (5), 4.4 (4)
      --
      Adding_OP := CD.Sy;
      InSymbol (CD);
      Term (FSys + Plus_Minus, X);
      --  At this point we have consumed "+X" or "-X".
      if Adding_OP = Plus and X.TYP = String_Literals then         --  +"Hello"
        Emit_Std_Funct (CD, SF_Literal_to_VString);
        Construct_Root (X, VStrings);
      elsif Adding_OP = Plus and X.TYP = Strings_as_VStrings then  --  +Enum'Image (x)
        Construct_Root (X, VStrings);
      elsif Adding_OP = Plus and X.TYP = Chars then                --  +'H'
        Emit_Std_Funct (CD, SF_Char_to_VString);
        Construct_Root (X, VStrings);
      elsif Adding_OP = Plus and then Is_Char_Array (CD, X) then   --  +S
        Emit_Std_Funct (CD,
          SF_String_to_VString,
          Operand_1_Type (CD.Arrays_Table (X.Ref).Array_Size)
        );
        Construct_Root (X, VStrings);
      elsif Adding_OP = Minus and X.TYP = VStrings then            --  -v
        Construct_Root (X, Strings_as_VStrings);
      elsif X.TYP not in Numeric_Typ then
        Error (CD, err_illegal_type_for_arithmetic_expression);
      elsif Adding_OP = Minus then
        Emit_Unary_Minus (CD, X.TYP);
        if X.TYP = Ints then
          Ranges.Negate_Range (CD, X);
        end if;
      end if;
    else
      Term (FSys + binary_adding_operator, X);
    end if;
    --
    --  We collect here eventual terms: a {+ b}      RM 4.4 (4)
    --
    while binary_adding_operator (CD.Sy) loop
      Adding_OP := CD.Sy;
      InSymbol (CD);
      Term (FSys + binary_adding_operator, y);
      if X.TYP = NOTYP or y.TYP = NOTYP then
        null;  --  Something is already wrong at this point; nothing to check or emit.
      else
        case Adding_OP is
          when OR_Symbol =>
            if X.TYP = Bools and y.TYP = Bools then
              Emit (CD, k_OR_Boolean);
            else
              Error (CD, err_resulting_type_should_be_Boolean);
              X.TYP := NOTYP;
            end if;
          when XOR_Symbol =>
            if X.TYP = Bools and y.TYP = Bools then
              Emit (CD, k_XOR_Boolean);
            else
              Error (CD, err_resulting_type_should_be_Boolean);
              X.TYP := NOTYP;
            end if;
          when Plus | Minus =>
            if X.TYP in Numeric_Typ and then y.TYP in Numeric_Typ then
              if X.TYP = y.TYP then
                Emit_Arithmetic_Binary_Instruction (CD, Adding_OP, X.TYP);
              else
                Forbid_Type_Coercion (CD, Adding_OP, X, y);
              end if;
              if X.TYP = Ints then
                if Ranges.Is_Singleton_Range (y, 0) then
                  --  X +/- 0 = X.
                  null;  --  Keep X's range.
                else
                  X.Construct_Root (X.TYP);  --  Forget subtype bounds
                end if;
              end if;
            elsif X.TYP = Times and y.TYP = Times and Adding_OP = Minus then
              Emit_Std_Funct (CD, SF_Time_Subtract);  --  T2 - T1
              Construct_Root (X, Durations);
            elsif X.TYP = Durations then
              if y.TYP = Floats then
                --  Duration hack for "X + 1.234" (see Delay_Statement
                --  for full explanation).
                Emit_Std_Funct (CD, SF_Float_to_Duration);
                Construct_Root (y, Durations);  --  Now X and Y have the type Duration.
              end if;
              if y.TYP = Durations then
                if Adding_OP = Plus then
                  Emit_Std_Funct (CD, SF_Duration_Add);
                else
                  Emit_Std_Funct (CD, SF_Duration_Subtract);
                end if;
              else
                Issue_Undefined_Operator_Error (CD, Adding_OP, X, y);
              end if;
            else
              Issue_Undefined_Operator_Error (CD, Adding_OP, X, y);
            end if;
          when Ampersand_Symbol =>
            if not (VString_Concatenation or else String_Concatenation) then
              Issue_Undefined_Operator_Error (CD, Adding_OP, X, y);
            end if;
          when others =>
            --  Doesn't happen: Binary_Adding_Operators(OP) is True.
            null;
        end case;
      end if;
    end loop;
  end Simple_Expression;

  procedure Boolean_Expression (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level;
    FSys  :        Defs.Symset;
    X     :    out Co_Defs.Exact_Subtyp
  )
  is
  begin
    Expression (CD, Level, FSys, X);
    Check_Boolean (CD, X.TYP);
  end Boolean_Expression;

  procedure Subtype_Prefixed_Expression (
    CD           : in out Co_Defs.Compiler_Data;
    Level        : in     Defs.Nesting_level;
    FSys         : in     Defs.Symset;
    Typ_ID_Index : in     Natural;
    X            : in out Co_Defs.Exact_Subtyp
  )
  is
    Mem_Sy : constant KeyWSymbol := CD.Sy;
  begin
    pragma Assert (CD.IdTab (Typ_ID_Index).entity = TypeMark);
    InSymbol (CD);
    case Mem_Sy is
      when LParent    =>  --  S (...)
        Type_Conversion (CD, Level, FSys, CD.IdTab (Typ_ID_Index), X);
      when Apostrophe =>  --  S'First, S'Image, ...
        Attributes.Subtype_Attribute (CD, Level, FSys, Typ_ID_Index, X);
      when others =>
        Error (CD, err_syntax_error, ": expected ""'"" or ""("" here", major);
    end case;
  end Subtype_Prefixed_Expression;

end HAC_Sys.Parser.Expressions;
