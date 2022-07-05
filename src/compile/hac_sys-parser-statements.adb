with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Multi_Precision_Integers,
     HAC_Sys.Parser.Calls,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Ranges,
     HAC_Sys.PCode,
     HAC_Sys.Parser.Standard_Procedures,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Statements is

  use type Defs.HAC_Integer;

  procedure Assignment
    (CD              : in out Co_Defs.Compiler_Data;
     FSys            :        Defs.Symset;
     Level           :        Defs.Nesting_level;
     Var_Id_Index    :        Integer;
     Check_read_only :        Boolean)
  is
    use Compiler.PCode_Emit, Co_Defs, Defs, Expressions, Helpers, PCode, Scanner, Errors;
    X, Y  : Exact_Subtyp;
    X_Len : Natural;
    procedure Issue_Type_Mismatch_Error is
    begin
      Type_Mismatch (CD, err_types_of_assignment_must_match, Found => Y, Expected => X);
    end Issue_Type_Mismatch_Error;
  begin
    pragma Assert (CD.IdTab (Var_Id_Index).entity = Variable);
    X := CD.IdTab (Var_Id_Index).xtyp;
    Emit_2
     (CD,
      (if CD.IdTab (Var_Id_Index).normal then
         k_Push_Address           --  Normal variable, we push its address
       else
         k_Push_Discrete_Value),  --  The value is a reference, we want that address.
      Operand_1_Type (CD.IdTab (Var_Id_Index).lev),
      Operand_2_Type (CD.IdTab (Var_Id_Index).adr_or_sz));
    if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
      --  Resolve composite types' selectors (arrays and records).
      Selector (CD, Level, Becomes_EQL + FSys, X);
      --  Now, X denotes the leaf type (which can be composite as well).
    end if;
    --  Parse the  ":="  of  "X := Y;"
    case CD.Sy is
      when Becomes =>
        InSymbol (CD);
      when EQL =>
        --  Common mistake by BASIC or C programmers.
        Error (CD, err_EQUALS_instead_of_BECOMES);
        InSymbol (CD);
      when others =>
        Error (CD, err_BECOMES_missing);
    end case;
    if Check_read_only and then CD.IdTab (Var_Id_Index).read_only then
      Error (CD, err_cannot_modify_constant_or_in_parameter);
    end if;
    Expression (CD, Level, Semicolon_Set, Y);
    --
    if X.TYP = Y.TYP and X.TYP /= NOTYP then
      if Discrete_Typ (X.TYP) then
        if Ranges.Do_Ranges_Overlap (X, Y) then
          if X.Discrete_First > Y.Discrete_First then
            Compiler.PCode_Emit.Emit_3
              (CD, k_Check_Lower_Bound, X.Discrete_First, Typen'Pos (X.TYP), Operand_3_Type (X.Ref));
          end if;
          if X.Discrete_Last < Y.Discrete_Last then
            Compiler.PCode_Emit.Emit_3
              (CD, k_Check_Upper_Bound, X.Discrete_Last, Typen'Pos (X.TYP), Operand_3_Type (X.Ref));
          end if;
        else
          Error
            (CD, err_range_constraint_error,
             "value of expression (" &
             (if Ranges.Is_Singleton_Range (Y) then
                --  More understandable message part for a single value
                Discrete_Image (CD, Y.Discrete_First, X.TYP, X.Ref)
              else
                "range: " &
                Discrete_Range_Image (CD, Y.Discrete_First, Y.Discrete_Last, X.TYP, X.Ref)) &
             ") is out the destination's range, " &
             Discrete_Range_Image (CD, X.Discrete_First, X.Discrete_Last, X.TYP, X.Ref),
             minor);
        end if;
      end if;
      if X.TYP in Standard_Typ then
        Emit_1 (CD, k_Store, Typen'Pos (X.TYP));
      elsif X.Ref /= Y.Ref then
        Issue_Type_Mismatch_Error;  --  E.g. different arrays, enums, ...
      else
        case X.TYP is
          when Arrays =>
            Emit_1 (CD, k_Copy_Block, Operand_2_Type (CD.Arrays_Table (X.Ref).Array_Size));
          when Records =>
            Emit_1 (CD, k_Copy_Block, Operand_2_Type (CD.Blocks_Table (X.Ref).VSize));
          when Enums =>
            --  Behaves like a "Standard_Typ".
            --  We have checked that X.Ref = Y.Ref (same actual type).
            Emit_1 (CD, k_Store, Typen'Pos (X.TYP));
          when others =>
            raise Internal_error with "Assignment: X := Y on unsupported Typ ?";
        end case;
      end if;
    else
      --
      --  Here, X.TYP and Y.TYP are different.
      --
      if X.TYP = Floats and Y.TYP = Ints then
        Forbid_Type_Coercion (CD, Found => Y, Expected => X);
      elsif X.TYP = Durations and Y.TYP = Floats then
        --  Duration hack (see Delay_Statement for full explanation).
        Emit_Std_Funct (CD, SF_Float_to_Duration);
        Emit_1 (CD, k_Store, Typen'Pos (X.TYP));
      elsif Is_Char_Array (CD, X) and Y.TYP = String_Literals then
        X_Len := CD.Arrays_Table (X.Ref).Array_Size;
        if X_Len = CD.SLeng then
          Emit_1 (CD, k_String_Literal_Assignment, Operand_2_Type (X_Len));
        else
          Error (CD, err_string_lengths_do_not_match,
            "variable has length" & Integer'Image (X_Len) &
            ", literal has length" & Integer'Image (CD.SLeng),
            minor
          );
        end if;
      elsif X.TYP = VStrings
        and then
          (Y.TYP in String_Literals | Strings_as_VStrings
           or else Is_Char_Array (CD, Y))
      then
        Error (CD, err_string_to_vstring_assignment);
      elsif X.TYP = NOTYP then
        if CD.error_count = 0 then
          raise Internal_error with "Assignment: assigned variable (X) is typeless";
        end if;
        --  All right, there were already enough compilation error messages...
      elsif Y.TYP = NOTYP then
        if CD.error_count = 0 then
          raise Internal_error with "Assignment: assigned value (Y) is typeless";
        end if;
        --  All right, there were already enough compilation error messages...
      else
        Issue_Type_Mismatch_Error;
        --  NB: We are in the X.TYP /= Y.TYP case.
      end if;
    end if;
  end Assignment;

  procedure Statement  --  Ada RM 5.1 (3)
    (CD         : in out Co_Defs.Compiler_Data;
     FSys_St    :        Defs.Symset;
     Block_Data : in out Block_Data_Type);

  procedure Sequence_of_Statements  --  Ada RM 5.1 (2)
    (CD         : in out Co_Defs.Compiler_Data;
     Sentinel   :        Defs.Symset;
     Block_Data : in out Block_Data_Type;
     Optional   :        Boolean := False)
  is
    use Defs, Helpers, Errors;
    statement_or_sentinel : constant Symset := Statement_Begin_Symbol or Sentinel;
  begin
    if Sentinel (CD.Sy) and then not Optional then
      --  GdM 15-Aug-2014: there should be at least one statement.
      --
      --  But in some places in the grammar the sequence is optional:
      --  in an accept_alternative and in a delay_alternative.
      --  In both cases the sequence follow a first statement (accept or delay).
      Error (CD, err_statement_expected, severity => minor);
    else
      loop
        Statement (CD, statement_or_sentinel, Block_Data);
        exit when Sentinel (CD.Sy) or CD.error_count > 0;
      end loop;
    end if;
  end Sequence_of_Statements;

  ------------------------------------------------------------------
  --------------------------------------Statement - Ada RM 5.1 (3)--
  procedure Statement
    (CD         : in out Co_Defs.Compiler_Data;
     FSys_St    :        Defs.Symset;
     Block_Data : in out Block_Data_Type)
  is
    use Compiler.PCode_Emit, Calls, Co_Defs, Defs, Enter_Def, Expressions,
        Helpers, PCode, Errors;
    use type Alfa;

    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;

    procedure Accept_Statement is            -- Hathorn

      procedure Accept_Call is
      begin   --  !!  Check to make sure parameters match with Entry Statement
        if CD.Sy = Semicolon then
          return;
        end if;
        if CD.Sy = LParent then          -- <--- temporary
          while not (CD.Sy = DO_Symbol or CD.Sy = RParent) loop
            InSymbol;
          end loop; -- !! should check no. and
        end if;    -- Types of parms.
        if CD.Sy = RParent then
          InSymbol;
        end if;
      end Accept_Call;

      I_Entry : Integer;
    begin  --  Accept_Statement
      InSymbol;
      I_Entry := Locate_Identifier (CD, CD.Id, Block_Data.level);
      if CD.IdTab (I_Entry).entity /= aEntry then
        Error (CD, err_syntax_error, ": an entry name is expected here");
      end if;
      InSymbol;
      Accept_Call;
      Emit_1 (CD, k_Accept_Rendezvous, Operand_2_Type (I_Entry));
      if CD.Sy = DO_Symbol then
        if Block_Data.level = Nesting_Level_Max then
          Fatal (LEVELS);  --  Exception is raised there.
        end if;
        Block_Data.level := Block_Data.level + 1;
        CD.Display (Block_Data.level) := CD.IdTab (I_Entry).block_or_pkg_ref;
        InSymbol;
        Sequence_of_Statements (CD, END_Set, Block_Data);
        Need_END_Symbol (CD);
        if CD.Sy = IDent then
          if CD.Id /= CD.IdTab (I_Entry).name then
            Error (CD, err_incorrect_name_after_END);
          end if;
          InSymbol;
        end if;
        Block_Data.level := Block_Data.level - 1;
      end if;
      Emit_1 (CD, k_End_Rendezvous, Operand_2_Type (I_Entry));
    end Accept_Statement;

    procedure Exit_Statement is
      --  Generate an absolute branch statement with a dummy end loop address
      X : Exact_Subtyp;
    begin
      pragma Assert (CD.Sy = EXIT_Symbol);
      InSymbol;  --  Consume EXIT symbol.
      if CD.Sy = WHEN_Symbol then  --  Conditional Exit
        InSymbol;
        Boolean_Expression (CD, Block_Data.level, Semicolon_Set, X);
        Emit_1 (CD, k_Jump_If_Zero_With_Pop, Operand_2_Type (CD.LC + 2));  --  Conditional jump around Exit
      end if;
      Emit_1 (CD, k_Jump, dummy_address_loop);  --  Unconditional jump with dummy address to be patched
    end Exit_Statement;

    procedure IF_Statement is
      X        : Exact_Subtyp;
      LC0, LC1 : Integer;
    begin
      InSymbol;
      Boolean_Expression (CD, Block_Data.level, FSys_St + DO_THEN, X);
      LC1 := CD.LC;
      Emit (CD, k_Jump_If_Zero_With_Pop);
      Need (CD, THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
      Sequence_of_Statements (CD, ELSE_ELSIF_END, Block_Data);
      LC0 := CD.LC;
      --
      while CD.Sy = ELSIF_Symbol loop  --  Added Hathorn
        InSymbol;
        Emit_1 (CD, k_Jump, dummy_address_if);  --  Unconditional jump with dummy address to be patched
        CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);       --  Patch the previous conditional jump
        Boolean_Expression (CD, Block_Data.level, FSys_St + DO_THEN, X);
        LC1 := CD.LC;
        Emit (CD, k_Jump_If_Zero_With_Pop);
        Need (CD, THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
        Sequence_of_Statements (CD, ELSE_ELSIF_END, Block_Data);  --  Statements after "ELSIF .. THEN".
      end loop;
      --
      if CD.Sy = ELSE_Symbol then
        InSymbol;
        Emit_1 (CD, k_Jump, dummy_address_if);  --  Jump to "END IF" - dummy address to be patched.
        CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);
        Sequence_of_Statements (CD, END_Set, Block_Data);  --  Statements after "ELSE".
      else
        CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);
      end if;
      Need (CD, END_Symbol, err_END_missing);         --  END (IF)
      Need (CD, IF_Symbol,  err_missing_closing_IF);  --  (END) IF
      --  Go back and patch the dummy addresses in unconditional jumps
      Patch_Addresses (CD.ObjCode (LC0 .. CD.LC), dummy_address_if);
    end IF_Statement;

    procedure LOOP_Statement (FCT_Loop_End : Opcode; B : Integer) is    -- Hathorn
      LC0 : constant Integer := CD.LC;
    begin
      if CD.Sy = LOOP_Symbol then
        InSymbol;
      else
        Skip (CD, Statement_Begin_Symbol, err_missing_closing_IF);
      end if;
      Sequence_of_Statements (CD, END_Set, Block_Data);
      Emit_1 (CD, FCT_Loop_End, Operand_2_Type (B));
      Need (CD, END_Symbol,  err_END_missing);           --  END (LOOP)
      Need (CD, LOOP_Symbol, err_closing_LOOP_missing);  --  (END) LOOP
      --  Go back and patch the dummy addresses generated by Exit statements.
      Patch_Addresses (CD.ObjCode (LC0 .. CD.LC), dummy_address_loop);
    end LOOP_Statement;

    procedure RETURN_Statement is           -- Hathorn
      --  Generate a procedure or function return Statement, calculate return value if req'D.
      X, Y : Exact_Subtyp;
      procedure Issue_Type_Mismatch_Error is
      begin
        Type_Mismatch (CD, err_type_of_return_statement_doesnt_match, Found => Y, Expected => X);
      end Issue_Type_Mismatch_Error;
    begin
      InSymbol;
      if CD.Sy = Semicolon then
        if Block_Data.entity = Funktion then
          Error (CD, err_functions_must_return_a_value);
        end if;
      else
        if Block_Data.entity = Prozedure then
          Error (CD, err_procedures_cannot_return_a_value, severity => major);
        end if;
        --  Calculate return value (destination: X; expression: Y).
        if CD.IdTab (Block_Data.block_id_index).block_or_pkg_ref /= CD.Display (Block_Data.level) then
          raise Program_Error with
            "Is it `return x` from main? Issue should have been caught earlier: " &
            "err_procedures_cannot_return_a_value.";
        end if;
        X := CD.IdTab (Block_Data.block_id_index).xtyp;
        Emit_2
          (CD,
           (if CD.IdTab (Block_Data.block_id_index).normal then
              k_Push_Address
            else
              k_Push_Value),
          Operand_1_Type (CD.IdTab (Block_Data.block_id_index).lev + 1),
          0);
        --
        Expression (CD, Block_Data.level, Semicolon_Set, Y);
        if X.TYP = Y.TYP then
          if (X.TYP in Standard_Typ)
            or else (X.TYP = Enums and then Exact_Typ (X) = Exact_Typ (Y))
          then
            Emit_1 (CD, k_Store, Typen'Pos (X.TYP));
          elsif X.Ref /= Y.Ref then
            Issue_Type_Mismatch_Error;
          end if;
        elsif X.TYP = Floats and Y.TYP = Ints then
          Forbid_Type_Coercion (CD, Found => Y, Expected => X);
        elsif X.TYP /= NOTYP and Y.TYP /= NOTYP then
          Issue_Type_Mismatch_Error;
        end if;
      end if;
      if Block_Data.entity = Funktion then
        Emit_1 (CD, k_Exit_Function, Normal_Procedure_Call);
      elsif Block_Data.is_main then
        Emit (CD, k_Halt_Interpreter);
      else
        Emit_1 (CD, k_Exit_Call, Normal_Procedure_Call);
      end if;
    end RETURN_Statement;

    procedure Delay_Statement is            -- Cramer. Generate a Task delay.
      Y : Exact_Subtyp;
    begin
      InSymbol;
      if CD.Sy = Semicolon then
        Skip (CD, Semicolon, err_missing_expression_for_delay);
      else
        Expression (CD, Block_Data.level, Semicolon_Set, Y);
        if Y.TYP /= Floats and Y.TYP /= Durations then
          Error (CD, err_wrong_type_in_DELAY);
        end if;
        if Y.TYP = Floats then
          --  Duration hack: for expressions like 2.0 * 3600.0, we don't
          --  know in advance it's not a Duration. Check with a "full
          --  Ada" compiler the type conformity of the expression.
          Emit_Std_Funct (CD, SF_Float_to_Duration);
        end if;
      end if;
      Emit (CD, k_Delay);
    end Delay_Statement;

    procedure CASE_Statement is  --  Ada RM 5.4
      X         : Exact_Subtyp;
      I, J, LC1 : Integer;
      CaseTab : array (1 .. Cases_Max) of CASE_Label_Value;
      ExitTab : array (1 .. Cases_Max) of Integer;
      others_flag : Boolean := False;
      use Multi_Precision_Integers;
      subtype Choice_Count_Type is Multi_Int (4);
      parsed_choices : Choice_Count_Type;

      function Count_Choices (Low, High : HAC_Integer) return Choice_Count_Type is
        result : Choice_Count_Type;
      begin
        pragma Assert (Low <= High);
        Fill (result, Multi (High) - Multi (Low) + 1);
        return result;
      end Count_Choices;

      procedure Discrete_Choice is  --  Ada RM 3.8.1 (5)
        label_1, label_2 : Constant_Rec;
        K : Integer;
        choice_symbol_set : constant Symset := FSys_St + Alt_Finger_THEN + Range_Double_Dot_Symbol;
      begin
        Static_Scalar_Expression (CD, Block_Data.level, choice_symbol_set, label_1);
        if CD.Sy = Range_Double_Dot_Symbol then
          --  !!  To do: non-explicit ranges, like a subtype name, a 'Range, ... .
          --      Ranges.Static_Range.
          InSymbol;
          Static_Scalar_Expression (CD, Block_Data.level, choice_symbol_set, label_2);
          if label_2.TP /= label_1.TP then
            Type_Mismatch (
              CD, err_case_label_not_same_type_as_case_clause,
              Found    => label_2.TP,
              Expected => label_1.TP
            );
          end if;
        else
          label_2 := label_1;
        end if;
        if Exact_Typ (label_1.TP) /= Exact_Typ (X) then
          Type_Mismatch (
            CD, err_case_label_not_same_type_as_case_clause,
            Found    => label_1.TP,
            Expected => X
          );
        elsif I = Cases_Max then
          Fatal (Case_Labels);  --  Exception is raised there.
        else
          if        (label_1.I not in X.Discrete_First .. X.Discrete_Last)
            or else (label_2.I not in X.Discrete_First .. X.Discrete_Last)
          then
            Error (CD, err_choice_out_of_range, severity => minor);
          end if;
          I := I + 1;
          CaseTab (I) := (value_1 => label_1.I, value_2 => label_2.I, LC => CD.LC, Is_others => False);
          K := 0;
          loop
            K := K + 1;
            --  Detect any range overlap.
            exit when
              Ranges.Do_Ranges_Overlap (label_1.I, label_2.I, CaseTab (K).value_1, CaseTab (K).value_2);
          end loop;
          if K < I then
            Error (CD, err_duplicate_case_choice_value);
          end if;
          --  Since single choices or ranges do not overlap,
          --  we can simply add the number of covered values in order to check
          --  at the end that everything is covered.
          Fill (parsed_choices, parsed_choices + Count_Choices (label_1.I, label_2.I));
        end if;
      end Discrete_Choice;

      procedure WHEN_Discrete_Choice_List is
      begin
        pragma Assert (CD.Sy = WHEN_Symbol);  --  One_Case called only on WHEN_Symbol.
        InSymbol;  --  Consume `WHEN`
        --  Here, a discrete_choice_list (Ada RM 3.8.1 (4)) following WHEN.
        if Constant_Definition_Begin_Symbol (CD.Sy) then
          if others_flag then  --  Normal choice list *atfer* the "others" choice.
            Error (CD, err_case_others_alone_last);
          end if;
          Discrete_Choice;
          while CD.Sy = Alt loop
            InSymbol;  --  Consume '|' symbol.
            if CD.Sy = OTHERS_Symbol then  --  "others" mixed with normal choices.
              Error (CD, err_case_others_alone_last);
            else
              Discrete_Choice;
            end if;
          end loop;
        elsif CD.Sy = OTHERS_Symbol then        -- Hathorn
          if others_flag then  --  Duplicate "others".
            Error (CD, err_case_others_alone_last);
          end if;
          others_flag := True;
          if I = Cases_Max then
            Fatal (Case_Labels);  --  Exception is raised there.
          end if;
          I := I + 1;
          CaseTab (I) := (value_1 | value_2 => 0, LC => CD.LC, Is_others => True);
          InSymbol;
        end if;
        if CD.Sy = THEN_Symbol then  --  Mistake happens when converting IF statements to CASE.
          Error (CD, err_THEN_instead_of_Arrow, severity => major);
          InSymbol;
        else
          Need (CD, Finger, err_FINGER_missing);
        end if;
        Sequence_of_Statements (CD, END_WHEN, Block_Data);
        J := J + 1;
        ExitTab (J) := CD.LC;
        Emit (CD, k_Jump);
      end WHEN_Discrete_Choice_List;

      procedure Check_Coverage is
        expected_choices, difference : Choice_Count_Type;
      begin
        pragma Assert (Choice_Count_Type'Size >= HAC_Integer'Size);
        if others_flag then
          return;
        end if;
        Fill (expected_choices, Count_Choices (X.Discrete_First, X.Discrete_Last));
        Fill (difference, expected_choices - parsed_choices);
        --  When difference < 0, choices are out of
        --  range (error detected on parsing).
        if difference > 0 then
          Error
            (CD, err_choices_not_covered,
             (if difference > 99 then ""
              elsif Equal (difference, 1) then ": one case is missing"
              else ":" & Basic_Int'Image (Basic (difference)) & " cases are missing"),
             minor);
        end if;
      end Check_Coverage;

    begin  --  CASE_Statement
      Fill (parsed_choices, 0);
      InSymbol;
      I := 0;
      J := 0;
      Expression (CD, Block_Data.level, FSys_St + Colon_Comma_IS_OF, X);
      if not Discrete_Typ (X.TYP) then
        Error (CD, err_bad_type_for_a_case_statement);
      end if;
      LC1 := CD.LC;
      Emit (CD, k_CASE_Switch);
      --  OF_Symbol was expected here in SmallAda! I.e. "case x OF when 1 => ..."
      case CD.Sy is
        when IS_Symbol =>
          InSymbol;
        when OF_Symbol =>
          Error (CD, err_OF_instead_of_IS);  --  Common mistake by Pascal programmers
          InSymbol;
        when others =>
          Error (CD, err_IS_missing);
      end case;
      if CD.Sy /= WHEN_Symbol then
        Error (CD, err_WHEN_missing, severity => major);
      end if;
      loop  --  All cases are parsed here.
        WHEN_Discrete_Choice_List;
        exit when CD.Sy /= WHEN_Symbol;
      end loop;
      Check_Coverage;
      CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);
      --  Set correct address for k_CASE_Switch above.
      --  This is the address of the following bunch of
      --  (CASE_Any_Choice, k_CASE_Match_Jump) pairs.
      for K in 1 .. I loop
        if CaseTab (K).Is_others then
          Emit (CD, k_CASE_Choice_Others);
        elsif CaseTab (K).value_1 = CaseTab (K).value_2 then
          Emit_1 (CD, k_CASE_Choice_Value, CaseTab (K).value_1);
        else
          Emit_2 (CD, k_CASE_Choice_Range, CaseTab (K).value_1, CaseTab (K).value_2);
        end if;
        Emit_1 (CD, k_CASE_Match_Jump, Operand_2_Type (CaseTab (K).LC));
      end loop;
      --  This is for having the interpreter exiting the k_CASE_Choice_Data loop.
      Emit (CD, k_CASE_No_Choice_Found);
      --
      for K in 1 .. J loop
        CD.ObjCode (ExitTab (K)).Y := Operand_2_Type (CD.LC);  --  Patch k_Jump addresses to after "END CASE;".
      end loop;
      Need (CD, END_Symbol,  err_END_missing);           --  END (CASE)
      Need (CD, CASE_Symbol, err_missing_closing_CASE);  --  (END) CASE
    end CASE_Statement;

    procedure WHILE_Statement is  --  RM 5.5 (8)
      X : Exact_Subtyp;
      LC_Cond_Eval, LC_Cond_Jump : Integer;
    begin
      InSymbol;  --  Consume WHILE symbol.
      LC_Cond_Eval := CD.LC;
      Boolean_Expression (CD, Block_Data.level, FSys_St + DO_LOOP, X);
      LC_Cond_Jump := CD.LC;
      Emit (CD, k_Jump_If_Zero_With_Pop);
      LOOP_Statement (k_Jump, LC_Cond_Eval);
      CD.ObjCode (LC_Cond_Jump).Y := Operand_2_Type (CD.LC);
    end WHILE_Statement;

    procedure FOR_Statement is  --  RM 5.5 (9)
      FOR_Begin_Instruction : Opcode; --  Forward  or  Reverse
      LC_FOR_Begin,
      Previous_Last : Index;
    begin
      --
      --  Pushed on the stack:
      --     - address of the loop parameter (a temporary iterator variable)
      --     - lower bound value
      --     - upper bound value
      --
      InSymbol;  --  Consume FOR symbol.
      if CD.Sy = IDent then
        if CD.Id_Count = Id_Table_Max then
          Fatal (IDENTIFIERS);  --  Exception is raised there.
        end if;
        --  Declare local loop control Variable  --  added Hathorn
        Previous_Last := CD.Blocks_Table (CD.Display (Block_Data.level)).Last_Id_Idx;
        CD.Id_Count := CD.Id_Count + 1;
        CD.IdTab (CD.Id_Count) :=        --  Loop parameter: the "i" in  "for i in 1..10 loop"
             (name             => CD.Id,
              name_with_case   => CD.Id_with_case,
              link             => Previous_Last,
              entity           => Variable,
              read_only        => True,
              decl_kind        => complete,
              xtyp             => Undefined,  --  Subtype is determined by the range.
              block_or_pkg_ref => 0,
              normal           => True,
              lev              => Block_Data.level,
              adr_or_sz        => Block_Data.data_allocation_index
             );
        CD.Blocks_Table (CD.Display (Block_Data.level)).Last_Id_Idx  := CD.Id_Count;
        Block_Data.data_allocation_index := Block_Data.data_allocation_index + 1;
        Block_Data.max_data_allocation_index :=
          Integer'Max (Block_Data.max_data_allocation_index, Block_Data.data_allocation_index);
        CD.Blocks_Table (CD.Display (Block_Data.level)).VSize := Block_Data.max_data_allocation_index;
      else
        Skip (CD, Fail_after_FOR + FSys_St, err_identifier_missing);
      end if;
      --
      Emit_2 (CD, k_Push_Address,
        Operand_1_Type (CD.IdTab (CD.Id_Count).lev),
        Operand_2_Type (CD.IdTab (CD.Id_Count).adr_or_sz)
      );
      InSymbol;
      FOR_Begin_Instruction := k_FOR_Forward_Begin;
      Need (CD, IN_Symbol, err_IN_missing);  --       "IN"  in  "for i in reverse 1 .. 10 loop"
      if CD.Sy = REVERSE_Symbol then         --  "REVERSE"  in  "for i in reverse 1 .. 10 loop"
        FOR_Begin_Instruction := k_FOR_Reverse_Begin;
        InSymbol;
      end if;
      Ranges.Dynamic_Range (CD, Block_Data.level, FSys_St,
        err_control_variable_of_the_wrong_type,
        CD.IdTab (CD.Id_Count).xtyp  --  Set the subtype of "C" in "for C in Red .. Blue loop"
      );
      LC_FOR_Begin := CD.LC;
      Emit (CD, FOR_Begin_Instruction);
      LOOP_Statement (For_END_Instruction (FOR_Begin_Instruction), CD.LC);
      CD.ObjCode (LC_FOR_Begin).Y := Operand_2_Type (CD.LC);  --  Address of the code just after the loop's end.
      --  Forget the loop parameter (the "iterator variable"):
      CD.Id_Count := CD.Id_Count - 1;
      CD.Blocks_Table (CD.Display (Block_Data.level)).Last_Id_Idx := Previous_Last;
      Block_Data.data_allocation_index := Block_Data.data_allocation_index - 1;
      --  The VM must also de-stack the 3 data pushed on the stack for the FOR loop:
      Emit (CD, k_FOR_Release_Stack_After_End);
    end FOR_Statement;

    procedure Select_Statement is
      procedure Select_Error (N : Compile_Error) is
      begin
        Skip (CD, Semicolon, N);
      end Select_Error;

      --  Either a Timed or Conditional Entry Call.

      procedure Qualified_Entry_Call is
        I, J, IStart, IEnd : Integer;
        patch              : array (0 .. 4) of Integer;
        O                  : Order;
        Y                  : Exact_Subtyp;
      begin
        I := Locate_Identifier (CD, CD.Id, Block_Data.level);
        if CD.IdTab (I).entity = aTask then
          InSymbol;
          Entry_Call (CD, Block_Data.level, FSys_St, I, -1);
          if CD.ObjCode (CD.LC - 2).F = k_Call then  --  Need to patch CallType later
            patch (0) := CD.LC - 2;
          else
            patch (0) := CD.LC - 3;
          end if;       -- LC-1 must be OP=3, update Display
          patch (1) := CD.LC;  --  Need to patch in JMPC address later
          Emit_1 (CD, k_Jump_If_Zero_With_Pop, dummy_address_if);  --  JMPC, address patched in after ELSE
                                    --  or OR
          if CD.Sy = Semicolon then
            InSymbol;
          else
            Skip (CD, Semicolon, err_semicolon_missing);
          end if;
          if CD.Sy not in OR_Symbol | ELSE_Symbol then
            Sequence_of_Statements (CD, ELSE_OR, Block_Data);
          end if;
          if CD.Sy = OR_Symbol then  --  =====================> Timed Entry Call
            CD.ObjCode (patch (0)).X     := Timed_Entry_Call;
            CD.ObjCode (patch (0) + 1).Y := Timed_Entry_Call;  --  Exit type matches Entry type
            InSymbol;
            if CD.Sy = DELAY_Symbol then
              InSymbol;
              if CD.Sy = Semicolon then
                Select_Error (err_missing_expression_for_delay);
              else          -- calculate delay value
                patch (2) := CD.LC;
                Expression (CD, Block_Data.level, Semicolon_Set, Y);
                patch (3) := CD.LC - 1;
                if Y.TYP /= Floats then
                  Select_Error (err_wrong_type_in_DELAY);
                else        --  end of timed Entry select ObjCode, do patching
                  CD.ObjCode (patch (1)).Y := Operand_2_Type (CD.LC);  --  if Entry not made, Skip rest
                  J                      := patch (3) - patch (2) + 1;
                  IStart                 := patch (0);
                  IEnd                   := CD.LC - 1;
                  while J > 0 loop           --  move delay time ObjCode To before
                    O := CD.ObjCode (IEnd);  --  opcodes kCall, k_Exit_Call
                    for I in reverse IEnd - 1 .. IStart loop
                      CD.ObjCode (I + 1) := CD.ObjCode (I);
                    end loop;
                    CD.ObjCode (IStart) := O;
                    J                   := J - 1;
                  end loop;
                  InSymbol;
                end if;
              end if;
            else
              Select_Error (err_expecting_DELAY);
            end if;
          --  end Sy = OrSy
          else              -- Sy = ELSE_Symbol, ===============> Conditional Entry Call
            CD.ObjCode (patch (0)).X     := Conditional_Entry_Call;
            CD.ObjCode (patch (0) + 1).Y := Conditional_Entry_Call;
            patch (2)                    := CD.LC;
            Emit_1 (CD, k_Jump, dummy_address_if);  -- JMP, address patched in after END SELECT
            patch (3) := CD.LC;
            InSymbol;
            Sequence_of_Statements (CD, END_Set, Block_Data);
            CD.ObjCode (patch (1)).Y  := Operand_2_Type (patch (3));
            CD.ObjCode (patch (2)).Y  := Operand_2_Type (CD.LC);
          end if;
          if CD.Sy /= END_Symbol then
            Select_Error (err_END_missing);
          end if;
          InSymbol;
          if CD.Sy /= SELECT_Symbol then
            Select_Error (err_SELECT_missing);
          end if;
        else
          Select_Error (err_expecting_task_entry);
        end if;          -- Task.Entry Call expected
      end Qualified_Entry_Call;

      procedure Selective_Wait is         -- Kurtz <===================
        --  Jay, this Buds for you !!
        JSD, Alt_Patch      : Fixed_Size_Patch_Table;
        ISD, IAlt, StartSel : Integer;
        SelectDone          : Boolean;
        X, Y                : Exact_Subtyp;
        do_terminate        : Boolean;

        procedure Accept_Statement_2 is      -- Kurtz

          procedure Accept_Call_2 is
          begin
            --  Check to make sure parameters match with Entry Statement
            if CD.Sy = Semicolon then
              return;
            end if;
            if CD.Sy = LParent then      -- should be modified
              --  To check no. and
              while not (CD.Sy = DO_Symbol or CD.Sy = RParent) loop
                InSymbol;
              end loop;
            end if;        -- of parameters.
            if CD.Sy = RParent then
              InSymbol;
            end if;
          end Accept_Call_2;

          I : Integer;
        begin         -- Accept_Statment_2
          InSymbol;
          I := Locate_Identifier (CD, CD.Id, Block_Data.level);
          if CD.IdTab (I).entity /= aEntry then
            Select_Error (err_syntax_error);
          end if;
          InSymbol;
          Accept_Call_2;
          Emit_2 (CD, k_Selective_Wait, 2, Operand_2_Type (I));      --  Retain Entry Index
          Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
          Emit_2 (CD, k_Selective_Wait, 3, Operand_2_Type (CD.LC));  --  ACCEPT IF Ready ELSE Skip To LC
          --  CONDITIONAL ACCEPT MUST BE ATOMIC
          if CD.Sy = DO_Symbol then
            if Block_Data.level = Nesting_Level_Max then
              Fatal (LEVELS);  --  Exception is raised there.
            end if;
            Block_Data.level := Block_Data.level + 1;
            CD.Display (Block_Data.level) := CD.IdTab (I).block_or_pkg_ref;
            InSymbol;
            Sequence_of_Statements (CD, END_Set, Block_Data);
            Need_END_Symbol (CD);
            if CD.Sy = IDent then
              if CD.Id /= CD.IdTab (I).name then
                Select_Error (err_incorrect_name_after_END);
              end if;
            end if;
            Block_Data.level := Block_Data.level - 1;
            InSymbol;
          end if;
          Emit_1 (CD, k_End_Rendezvous, Operand_2_Type (I));
        end Accept_Statement_2;

      begin  --  Selective_Wait ===============================> Kurtz
        ISD          := 0;
        IAlt         := 0;
        SelectDone   := False;
        do_terminate := False;
        StartSel     := CD.LC;
        Emit_2 (CD, k_Selective_Wait, 1, 0);  --  START OF SELECT SELECTIVE Wait SEQUENCE
        loop
          case CD.Sy is
            when WHEN_Symbol =>
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              InSymbol;  --  Consume WHEN symbol.
              Boolean_Expression (CD, Block_Data.level, FSys_St + Finger, X);
              InSymbol;
              case CD.Sy is
                when ACCEPT_Symbol =>
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump_If_Zero_With_Pop);
                  Accept_Statement_2;
                when DELAY_Symbol =>
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump_If_Zero_With_Pop);
                  InSymbol;
                  Expression (CD, Block_Data.level, FSys_St + Semicolon, Y);
                  Emit_2 (CD, k_Selective_Wait, 4, Operand_2_Type (CD.LC + 2));  --  Update delay time
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  end if;
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump);
                when others =>
                  Select_Error (err_missing_a_procedure_declaration);  --  ??
              end case;
              InSymbol;
              Sequence_of_Statements (CD, ELSE_END_OR, Block_Data, True);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);          --  patch JMP ADDRESS AT EndSy
            --  end WHEN_Symbol

            when ACCEPT_Symbol =>
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              Accept_Statement_2;
              InSymbol;
              Sequence_of_Statements (CD, ELSE_END_OR, Block_Data, True);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);

            when OR_Symbol =>
              InSymbol;  --  Consume OR symbol.

            when ELSE_Symbol =>
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              InSymbol;
              Sequence_of_Statements (CD, END_Set, Block_Data);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);

            when DELAY_Symbol =>
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              --  Generate a Task delay, calculate return value if req'D
              InSymbol;
              if CD.Sy = Semicolon then
                Skip (CD, Semicolon, err_missing_expression_for_delay);
              else          -- calculate return value
                Expression (CD, Block_Data.level, Semicolon_Set, Y);
                Emit_2 (CD, k_Selective_Wait, 4, Operand_2_Type (CD.LC + 2));  --  Update delay time
                if Y.TYP /= Floats then
                  Select_Error (err_wrong_type_in_DELAY);
                end if;
                Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                Emit (CD, k_Jump);
              end if;
              InSymbol;
              Sequence_of_Statements (CD, ELSE_END_OR, Block_Data, True);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);

            when TERMINATE_Symbol =>
              InSymbol;
              if CD.Sy /= Semicolon then
                Select_Error (err_semicolon_missing);
              end if;
              do_terminate := True;        -- Oguz
              InSymbol;

            when END_Symbol =>
              InSymbol;
              if CD.Sy /= SELECT_Symbol then
                Select_Error (err_END_missing);
              end if;
              SelectDone := True;
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              if do_terminate then
                Emit_2 (CD, k_Selective_Wait, 5, Operand_2_Type (StartSel));
              else
                Emit_2 (CD, k_Selective_Wait, 6, Operand_2_Type (StartSel));
              end if;   -- Suspend
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), JSD, ISD);

            when others =>
              SelectDone := True;
          end case;
          exit when SelectDone;
        end loop;
      end Selective_Wait;

    begin
      InSymbol;  --  Consume SELECT symbol.
      case CD.Sy is
        when ACCEPT_Symbol | WHEN_Symbol =>
          Selective_Wait;
          InSymbol;
        when IDent =>  --  Task Entry objectName.
          Qualified_Entry_Call;
          InSymbol;
          --  Timed or Conditional Entry Call (?)
        when others =>
          Select_Error (err_expecting_accept_when_or_entry_id);
      end case;
    end Select_Statement;

    procedure Block_Statement (block_name : Alfa) is  --  RM: 5.6
      block_statement_data : Block_Data_Type;
    begin
      Error (
        CD, err_not_yet_implemented,
        hint => "Block statements don't work yet",
        severity => major
      );
      --
      block_statement_data.level                         := Block_Data.level + 1;
      block_statement_data.block_id_index                := CD.Id_Count;
      block_statement_data.entity                        := Block_Data.entity;
      block_statement_data.is_main                       := False;
      block_statement_data.previous_declaration_id_index := No_Id;
      Block (CD, FSys_St, True, block_statement_data, block_name, block_name);  --  !! up/low case
      --
      --  !! to check:
      --  !! * stack management of variables when entering / quitting the block
      --  !! * object code and nesting... works on some cases at least (test.adb) !...
      --  !! Perhaps keep same level but have local declarations as for the
      --    variable in a FOR_Statement.
      --  !! Either both FOR and Block statements forget their definitions, or there
      --    is perhaps a problem with the stack (DX).
      --  !! Local bodies of subprograms surely mess the object code.
    end Block_Statement;

    procedure Named_Statement is
      --  Block_Statement or loop, named by "name: loop"
      new_ident_for_statement           : constant Alfa := CD.Id;
      new_ident_for_statement_with_case : constant Alfa := CD.Id_with_case;
      --
      procedure Check_ID_after_END_LOOP is  --  RM 5.5 (5)
      begin
        if CD.Sy = IDent then
          if CD.Id /= new_ident_for_statement then
            Error (CD, err_END_LOOP_ident_wrong,
                   hint => A2S (new_ident_for_statement_with_case));
          end if;
          InSymbol;  --  Consume identifier.
        else
          Error (CD, err_END_LOOP_ident_missing,
                 hint => A2S (new_ident_for_statement_with_case));
        end if;
      end Check_ID_after_END_LOOP;
      --
      dummy_idx : Natural;
    begin
      Enter (CD, Block_Data.level, new_ident_for_statement, CD.Id_with_case, Label, dummy_idx);
      if CD.Sy /= Colon then
        Error (CD, err_colon_missing_for_named_statement, A2S (CD.Id_with_case), major);
      end if;
      InSymbol;  --  Consume ':' symbol.
      case CD.Sy is
        when BEGIN_Symbol | DECLARE_Symbol => -- Named Block_Statement
          Block_Statement (new_ident_for_statement);
        when FOR_Symbol =>
          FOR_Statement;
          Check_ID_after_END_LOOP;
        when LOOP_Symbol =>
          LOOP_Statement (k_Jump, CD.LC);
          Check_ID_after_END_LOOP;
        when WHILE_Symbol =>
          WHILE_Statement;
          Check_ID_after_END_LOOP;
        when others =>
          Error (CD, err_syntax_error);
      end case;
    end Named_Statement;

    I_Statement : Integer;

  begin  --  Statement
    if CD.error_count > 0 then
      return;
    end if;
    if Statement_Begin_Symbol (CD.Sy) then
      case CD.Sy is
        when IDent =>
          I_Statement := Locate_Identifier (CD, CD.Id, Block_Data.level, Fail_when_No_Id => False);
          InSymbol;
          if I_Statement = No_Id then
            --  New identifier: must be an identifier for a named Block_Statement or loop.
            Named_Statement;
          else
            case CD.IdTab (I_Statement).entity is
              when Variable =>
                Assignment (CD, FSys_St, Block_Data.level, I_Statement, Check_read_only => True);
              when Declared_Number_or_Enum_Item =>
                Error (CD, err_illegal_statement_start_symbol, "constant or an enumeration item",
                       major);
              when TypeMark =>
                Error (CD, err_illegal_statement_start_symbol, "type name", major);
              when Funktion | Funktion_Intrinsic =>
                Error (CD, err_illegal_statement_start_symbol, "function name", major);
              when aTask =>
                Entry_Call (CD, Block_Data.level, FSys_St, I_Statement, Normal_Entry_Call);
              when Prozedure =>
                Subprogram_or_Entry_Call (CD, Block_Data.level, FSys_St, I_Statement, Normal_Procedure_Call);
              when Prozedure_Intrinsic =>
                Standard_Procedures.Standard_Procedure
                  (CD, Block_Data.level, FSys_St, SP_Code'Val (CD.IdTab (I_Statement).adr_or_sz));
              when Label =>
                Error (CD, err_duplicate_label, A2S (CD.Id));
                Test (CD, Colon_Set, FSys_St, err_colon_missing);
                InSymbol;
              when Paquetage =>
                Error (CD, err_illegal_statement_start_symbol, "package name", major);
              when others =>
                Error
                  (CD, err_syntax_error,
                   ". Entity found: " & Entity_Kind'Image (CD.IdTab (I_Statement).entity), major);
            end case;
          end if;  --  end IDent
        when ACCEPT_Symbol =>
          Accept_Statement;
        when BEGIN_Symbol | DECLARE_Symbol =>
          --  Anonymous Block Statement
          Block_Statement (Empty_Alfa);
        when CASE_Symbol =>
          CASE_Statement;
        when DELAY_Symbol =>
          Delay_Statement;
        when EXIT_Symbol =>
          Exit_Statement;
        when FOR_Symbol =>
          FOR_Statement;
        when IF_Symbol =>
          IF_Statement;
        when LOOP_Symbol =>
          LOOP_Statement (k_Jump, CD.LC);
        when NULL_Symbol =>
          InSymbol;  --  Just consume the NULL symbol.
        when RETURN_Symbol =>
          RETURN_Statement;
        when SELECT_Symbol =>
          Select_Statement;
        when WHILE_Symbol =>
          WHILE_Statement;
        when others =>
          null;
      end case;
      --
      Need_Semicolon (CD);
    end if;  --  CD.Sy in Statement_Begin_Symbol
    --
    Test (CD, FSys_St - Semicolon, Semicolon_Set, err_incorrectly_used_symbol);
  end Statement;

end HAC_Sys.Parser.Statements;
