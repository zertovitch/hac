with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Calls,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Ranges,
     HAC_Sys.PCode,
     HAC_Sys.Parser.Standard_Procedures,
     HAC_Sys.Parser.Statements.CASE_Statement,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Statements is

  use type Defs.HAC_Integer;

  procedure Assignment
    (CD                : in out Co_Defs.Compiler_Data;
     FSys              :        Defs.Symset;
     context           :        Defs.Flow_Context;
     var_id_index      :        Integer;
     check_is_variable :        Boolean)
  is
    use Compiler.PCode_Emit, Co_Defs, Defs, Expressions, Helpers, PCode, Scanner, Errors;
    var : Identifier_Table_Entry renames CD.id_table (var_id_index);
    X, Y  : Exact_Subtyp;
    X_Len : Natural;
    --
    procedure Issue_Type_Mismatch_Error is
    begin
      Type_Mismatch (CD, err_types_of_assignment_must_match, Found => Y, Expected => X);
    end Issue_Type_Mismatch_Error;
    --
  begin
    pragma Assert (var.entity in Object_Kind);
    X := var.xtyp;
    Emit_2
     (CD,
      (if var.normal then
         k_Push_Address           --  Normal variable, we push its address
       else
         k_Push_Discrete_Value),  --  The value is a reference, we want that address.
      Operand_1_Type (var.lev),
      Operand_2_Type (var.adr_or_sz));
    if Selector_Symbol_Loose (CD.Sy) then  --  '.' or '(' or (wrongly) '['
      --  Resolve composite types' selectors (arrays and records).
      Selector (CD, context, Becomes_EQL + FSys, X);
      --  Now, X denotes the leaf type (which can be composite as well).
    end if;
    --  Parse the  ":="  of  "X := Y;"
    case CD.Sy is
      when Becomes =>
        In_Symbol (CD);
      when EQL =>
        --  Common mistake by BASIC or C programmers.
        Error (CD, err_EQUALS_instead_of_BECOMES);
        In_Symbol (CD);
      when others =>
        Error (CD, err_BECOMES_missing);
    end case;

    if check_is_variable and then var.entity = constant_object then
      Error (CD, err_cannot_modify_constant_or_in_parameter);
    end if;

    Expression (CD, context, Semicolon_Set, Y);
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
             ") is out of destination's range, " &
             Discrete_Range_Image (CD, X.Discrete_First, X.Discrete_Last, X.TYP, X.Ref),
             severity => minor);
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
            severity => minor);
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
     block_data : in out Block_Data_Type);

  procedure Sequence_of_Statements  --  Ada RM 5.1 (2)
    (CD         : in out Co_Defs.Compiler_Data;
     sentinel   :        Defs.Symset;
     block_data : in out Block_Data_Type;
     optional   :        Boolean := False)
  is
    use Defs, Helpers, Errors;
    statement_or_sentinel : constant Symset := Statement_Begin_Symbol or sentinel;
  begin
    if sentinel (CD.Sy) and then not optional then
      --  GdM 15-Aug-2014: there should be at least one statement.
      --
      --  But in some places in the grammar the sequence is optional:
      --  in an accept_alternative and in a delay_alternative.
      --  In both cases the sequence follow a first statement (accept or delay).
      Error (CD, err_statement_expected, severity => minor);
    else
      loop
        Statement (CD, statement_or_sentinel, block_data);
        exit when sentinel (CD.Sy) or else CD.error_count > 0;
      end loop;
    end if;
  end Sequence_of_Statements;

  procedure Sequence_of_Statements_in_a_Conditional_Statement
    (CD         : in out Co_Defs.Compiler_Data;
     sentinel   :        Defs.Symset;
     block_data : in out Block_Data_Type;
     optional   :        Boolean := False)
  is
    old_context : constant Defs.Flow_Context := block_data.context;
  begin
    block_data.context.is_within_condition := True;
    block_data.context.is_in_cond_within_loop := block_data.context.is_within_loop;
    --
    Sequence_of_Statements (CD, sentinel, block_data, optional);
    --
    block_data.context := old_context;
  end Sequence_of_Statements_in_a_Conditional_Statement;

  ------------------------------------------------------------------
  --------------------------------------Statement - Ada RM 5.1 (3)--
  procedure Statement
    (CD         : in out Co_Defs.Compiler_Data;
     FSys_St    :        Defs.Symset;
     block_data : in out Block_Data_Type)
  is
    use Compiler.PCode_Emit, Calls, Co_Defs, Defs, Enter_Def, Expressions,
        Helpers, PCode, Errors;
    use type Alfa;

    procedure In_Symbol is begin Scanner.In_Symbol (CD); end In_Symbol;

    procedure Accept_Statement is            -- Hathorn

      procedure Accept_Call is
      begin   --  !!  Check to make sure parameters match with Entry Statement
        if CD.Sy = Semicolon then
          return;
        end if;
        if CD.Sy = LParent then          -- <--- temporary
          while not (CD.Sy = DO_Symbol or CD.Sy = RParent) loop
            In_Symbol;
          end loop; -- !! should check no. and
        end if;    -- Types of parms.
        if CD.Sy = RParent then
          In_Symbol;
        end if;
      end Accept_Call;

      I_Entry : Integer;
    begin  --  Accept_Statement
      In_Symbol;
      I_Entry := Locate_CD_Id (CD, block_data.context.level);
      if CD.id_table (I_Entry).entity /= entree then
        Error (CD, err_general_error, "an entry name is expected here");
      else
        CD.target.Mark_Reference (I_Entry);
      end if;
      In_Symbol;
      Accept_Call;
      Emit_1 (CD, k_Accept_Rendezvous, Operand_2_Type (I_Entry));
      if CD.Sy = DO_Symbol then
        if block_data.context.level = nesting_level_max then
          Fatal (LEVELS);  --  Exception is raised there.
        end if;
        block_data.context.level := block_data.context.level + 1;
        CD.Display (block_data.context.level) := CD.id_table (I_Entry).block_or_pkg_ref;
        In_Symbol;
        Sequence_of_Statements (CD, END_Set, block_data);
        Need_END_Symbol (CD);
        if CD.Sy = IDent then
          if CD.Id /= CD.id_table (I_Entry).name then
            Error (CD, err_incorrect_name_after_END);
          else
            CD.target.Mark_Reference (I_Entry);
          end if;
          In_Symbol;
        end if;
        block_data.context.level := block_data.context.level - 1;
      end if;
      Emit_1 (CD, k_End_Rendezvous, Operand_2_Type (I_Entry));
    end Accept_Statement;

    procedure Exit_Statement is  --  RM 5.7
      --  Generate an absolute branch statement with a dummy end loop address
      X : Exact_Subtyp;
      exit_level : Natural := CD.loop_nesting_level;
      id_found : Boolean;
      count_FOR_loops : Natural := 0;
      landing_after_jump : Integer;
      conditional : Boolean := False;
    begin
      pragma Assert (CD.Sy = EXIT_Symbol);
      In_Symbol;  --  Consume EXIT symbol.
      if CD.loop_nesting_level = 0 then
        Error
          (CD,
           err_general_error,
           """exit"" without a ""loop"" - was ""return"" intended?",
           severity => major);
        --  Exception raised, compilation stopped.
      end if;
      --  Possible name:
      if CD.Sy = IDent then
        id_found := False;
        for level in reverse 1 .. CD.loop_nesting_level loop
          if CD.Nested_Loop_Table (level).is_FOR_loop then
            count_FOR_loops := count_FOR_loops + 1;
          end if;
          if CD.Nested_Loop_Table (level).loop_Id /= No_Id
            and then CD.Id = CD.id_table (CD.Nested_Loop_Table (level).loop_Id).name
          then
            id_found := True;
            exit_level := level;
            exit;
          end if;
        end loop;
        if not id_found then
          Error (CD, err_general_error, "loop name not matched: " & A2S (CD.Id_with_case));
        end if;
        In_Symbol;  --  Consume the identifier.
      else
        --  EXIT without a name.
        if CD.Nested_Loop_Table (CD.loop_nesting_level).is_FOR_loop then
          count_FOR_loops := 1;
        end if;
      end if;
      if CD.Sy = WHEN_Symbol then  --  Conditional Exit
        In_Symbol;  --  Consume WHEN symbol.
        Boolean_Expression (CD, block_data.context, Semicolon_Set, X);
        --  On False, conditional jump around Exit:
        if count_FOR_loops = 0 then
          landing_after_jump := CD.LC + 2;
        else
          landing_after_jump := CD.LC + 3;
        end if;
        Emit_1 (CD, k_Jump_If_Zero_With_Pop, Operand_2_Type (landing_after_jump));
        conditional := True;
      end if;
      --  De-stacking for all FOR loops.
      if count_FOR_loops > 0 then
        Emit_1 (CD, k_FOR_Release_Stack, Operand_2_Type (count_FOR_loops));
      end if;
      --  Unconditional jump with dummy address to be patched:
      Emit_1 (CD, k_Jump, dummy_address_loop - Operand_2_Type (exit_level));
      pragma Assert ((not conditional) or else CD.LC = landing_after_jump);
    end Exit_Statement;

    procedure IF_Statement is
      X        : Exact_Subtyp;
      LC0, LC1 : Integer;
    begin
      In_Symbol;
      Boolean_Expression (CD, block_data.context, FSys_St + DO_THEN, X);
      LC1 := CD.LC;
      Emit (CD, k_Jump_If_Zero_With_Pop);
      Need (CD, THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
      --  Statements after "THEN":
      Sequence_of_Statements_in_a_Conditional_Statement (CD, ELSE_ELSIF_END, block_data);
      LC0 := CD.LC;
      --
      while CD.Sy = ELSIF_Symbol loop
        In_Symbol;
        Emit_1 (CD, k_Jump, dummy_address_if);  --  Unconditional jump with dummy address to be patched
        CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);       --  Patch the previous conditional jump
        Boolean_Expression (CD, block_data.context, FSys_St + DO_THEN, X);
        LC1 := CD.LC;
        Emit (CD, k_Jump_If_Zero_With_Pop);
        Need (CD, THEN_Symbol, err_THEN_missing, Forgive => DO_Symbol);
        --  Statements after "ELSIF .. THEN":
        Sequence_of_Statements_in_a_Conditional_Statement (CD, ELSE_ELSIF_END, block_data);
      end loop;
      --
      if CD.Sy = ELSE_Symbol then
        In_Symbol;
        Emit_1 (CD, k_Jump, dummy_address_if);  --  Jump to "END IF" - dummy address to be patched.
        CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);
        --  Statements after "ELSE":
        Sequence_of_Statements_in_a_Conditional_Statement (CD, END_Set, block_data);
      else
        CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);
      end if;
      Need (CD, END_Symbol, err_END_missing);           --  END (if)
      if CD.Sy in CASE_Symbol | LOOP_Symbol | IDent then
        Error (CD, err_missing_closing_IF_2, severity => minor);
        In_Symbol;
      else
        Need (CD, IF_Symbol,  err_missing_closing_IF);  --  (end) IF
      end if;
      --  Go back and patch the dummy addresses in unconditional jumps
      Patch_Addresses (CD.ObjCode (LC0 .. CD.LC), dummy_address_if);
    end IF_Statement;

    procedure LOOP_Statement
      (FCT_Loop_End : Opcode;  --  Instruction for jumping back to loop begin.
       LC_At_Begin  : Integer;
       info         : Loop_Info)
    is
      LC0 : constant Integer := CD.LC;
      old_is_within_loop : constant Boolean := block_data.context.is_within_loop;
    begin
      CD.loop_nesting_level := CD.loop_nesting_level + 1;
      if CD.loop_nesting_level > loop_nesting_max then
        Fatal (Loop_Nesting_Levels);  --  Exception is raised there.
      end if;
      CD.Nested_Loop_Table (CD.loop_nesting_level) := info;
      if CD.Sy = LOOP_Symbol then
        In_Symbol;
      else
        Error_then_Skip (CD, Statement_Begin_Symbol, err_missing_closing_IF);
      end if;
      block_data.context.is_within_loop := True;
      Sequence_of_Statements (CD, END_Set, block_data);
      block_data.context.is_within_loop := old_is_within_loop;
      Emit_1 (CD, FCT_Loop_End, Operand_2_Type (LC_At_Begin));
      Need (CD, END_Symbol,  err_END_missing);             --  END (loop)
      if CD.Sy in CASE_Symbol | IF_Symbol | IDent then
        Error (CD, err_missing_closing_LOOP_2, severity => minor);
        In_Symbol;
      else
        Need (CD, LOOP_Symbol, err_missing_closing_LOOP);  --  (end) LOOP
      end if;
      --  Go back and patch the dummy addresses generated by Exit statements.
      Patch_Addresses
        (CD.ObjCode (LC0 .. CD.LC), dummy_address_loop - Operand_2_Type (CD.loop_nesting_level));
      CD.loop_nesting_level := CD.loop_nesting_level - 1;
    end LOOP_Statement;

    procedure RETURN_Statement is           -- Hathorn
      --  Generate a procedure or function return Statement, calculate return value if req'D.
      X, Y : Exact_Subtyp;
      procedure Issue_Type_Mismatch_Error is
      begin
        Type_Mismatch (CD, err_type_of_return_statement_doesnt_match, Found => Y, Expected => X);
      end Issue_Type_Mismatch_Error;
    begin
      In_Symbol;
      if CD.Sy = Semicolon then
        --  RETURN;
        if block_data.entity = funktion then
          Error (CD, err_functions_must_return_a_value);
        end if;
      else
        --  RETURN x;
        if block_data.entity = prozedure then
          Error (CD, err_procedures_cannot_return_a_value, severity => major);
        end if;
        block_data.return_statement_seen := True;
        --  Calculate return value (destination: X; expression: Y).
        if CD.id_table (block_data.block_id_index).block_or_pkg_ref /= CD.Display (block_data.context.level) then
          raise Program_Error with
            "Is it `return x` from main? Issue should have been caught earlier: " &
            "err_procedures_cannot_return_a_value.";
        end if;
        X := CD.id_table (block_data.block_id_index).xtyp;
        Emit_2
          (CD,
           (if CD.id_table (block_data.block_id_index).normal then
              k_Push_Address
            else
              k_Push_Value),
          Operand_1_Type (CD.id_table (block_data.block_id_index).lev + 1),
          0);
        --
        Expression (CD, block_data.context, Semicolon_Set, Y);
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
      if block_data.entity = funktion then
        Emit_1 (CD, k_Return_Function, Normal_Procedure_Call);
      elsif block_data.is_main then
        CD.target.Emit_Halt;
      else
        Emit_1 (CD, k_Return_Call, Normal_Procedure_Call);
      end if;
    end RETURN_Statement;

    procedure Delay_Statement is            -- Cramer. Generate a Task delay.
      Y : Exact_Subtyp;
    begin
      In_Symbol;
      if CD.Sy = Semicolon then
        Error_then_Skip (CD, Semicolon, err_missing_expression_for_delay);
      else
        Expression (CD, block_data.context, Semicolon_Set, Y);
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

    procedure WHILE_Statement (loop_Id : Natural) is  --  RM 5.5 (8)
      X : Exact_Subtyp;
      LC_Cond_Eval, LC_Cond_Jump : Integer;
      line_number : constant Natural := CD.CUD.location.line;
    begin
      In_Symbol;  --  Consume WHILE symbol.
      LC_Cond_Eval := CD.LC;
      Boolean_Expression (CD, block_data.context, FSys_St + DO_LOOP, X);
      LC_Cond_Jump := CD.LC;
      Emit (CD, k_Jump_If_Zero_With_Pop);
      LOOP_Statement (k_Jump, LC_Cond_Eval, (loop_Id, False, line_number));
      CD.ObjCode (LC_Cond_Jump).Y := Operand_2_Type (CD.LC);
    end WHILE_Statement;

    procedure FOR_Statement (label_Id : Natural) is  --  RM 5.5 (9)
      FOR_Begin_Instruction : Opcode;  --  Forward  or  Reverse
      LC_FOR_Begin,
      loop_param_id_index,
      previous_last : Index;
      loop_param_id, loop_param_id_with_case : Alfa;
      line_number : constant Natural := CD.CUD.location.line;
    begin
      --
      --  Pushed on the stack:
      --     - address of the loop parameter (a temporary iterator variable)
      --     - lower bound value
      --     - upper bound value
      --
      In_Symbol;  --  Consume FOR symbol.
      if CD.Sy = IDent then

        if CD.Id_Count = Id_Table_Max then
          Fatal (IDENTIFIERS);  --  Exception is raised there.
        end if;

        --  Declare local loop control Variable  --  added Hathorn
        previous_last := CD.Blocks_Table (CD.Display (block_data.context.level)).Last_Id_Idx;
        CD.Id_Count := CD.Id_Count + 1;

        loop_param_id_index     := CD.Id_Count;
        loop_param_id           := CD.Id;
        loop_param_id_with_case := CD.Id_with_case;

        CD.id_table (loop_param_id_index) :=  --  Loop parameter: the "i" in  "for i in 1..10 loop"
          (name                  => Empty_Alfa,  --  Hide name because of "for i in 1 .. i loop"
           name_with_case        => Empty_Alfa,
           link                  => previous_last,
           entity                => constant_object,
           decl_kind             => complete,
           xtyp                  => undefined_subtyp,  --  Subtype is determined by the range.
           block_or_pkg_ref      => 0,
           normal                => True,
           lev                   => block_data.context.level,
           adr_or_sz             => HAC_Integer (block_data.data_allocation_index),
           is_referenced         => False,
           is_read               => no,
           is_written_after_init => yes,
           is_initialized        => explicit,
           location              => (0, 0, 0));

        CD.target.Mark_Declaration;
        CD.Blocks_Table (CD.Display (block_data.context.level)).Last_Id_Idx  := loop_param_id_index;
        block_data.data_allocation_index := block_data.data_allocation_index + 1;
        block_data.max_data_allocation_index :=
          Integer'Max (block_data.max_data_allocation_index, block_data.data_allocation_index);
        CD.Blocks_Table (CD.Display (block_data.context.level)).VSize := block_data.max_data_allocation_index;
      else
        Error (CD, err_identifier_missing, severity => major);
      end if;

      Emit_2
        (CD, k_Push_Address,
         Operand_1_Type (CD.id_table (loop_param_id_index).lev),
         Operand_2_Type (CD.id_table (loop_param_id_index).adr_or_sz));
      In_Symbol;
      FOR_Begin_Instruction := k_FOR_Forward_Begin;
      Need (CD, IN_Symbol, err_IN_missing);  --       "IN"  in  "for i in reverse 1 .. 10 loop"
      if CD.Sy = REVERSE_Symbol then         --  "REVERSE"  in  "for i in reverse 1 .. 10 loop"
        FOR_Begin_Instruction := k_FOR_Reverse_Begin;
        In_Symbol;
      end if;
      Ranges.Dynamic_Range
        (CD, block_data.context, FSys_St,
         err_control_variable_of_the_wrong_type,
         CD.id_table (loop_param_id_index).xtyp);
         --  ^ Last parameter: set the subtype of "c" in "for c in Red .. Blue loop"
      LC_FOR_Begin := CD.LC;
      Emit (CD, FOR_Begin_Instruction);
      CD.id_table (loop_param_id_index).name           := loop_param_id;            --  Unhide name
      CD.id_table (loop_param_id_index).name_with_case := loop_param_id_with_case;  --  Unhide name

      LOOP_Statement
        (For_END_Instruction (FOR_Begin_Instruction), CD.LC, (label_Id, True, line_number));

      --  Patch the loop control exit address; points to the code
      --  just after the loop's end:
      CD.ObjCode (LC_FOR_Begin).Y := Operand_2_Type (CD.LC);
      block_data.data_allocation_index := block_data.data_allocation_index - 1;
      --
      --  Make the loop parameter unusable. The .name_with_case is still
      --  visible for the compiler dump. Previously, the identifier counter
      --  was decreased, but that method conflicted with the necessity of
      --  keeping all loop names within a block.
      CD.id_table (loop_param_id_index).name := Empty_Alfa;
    end FOR_Statement;

    procedure Select_Statement is
      procedure Select_Error (N : Compile_Diagnostic) is
      begin
        Error_then_Skip (CD, Semicolon, N);
      end Select_Error;

      --  Either a Timed or Conditional Entry Call.

      procedure Qualified_Entry_Call is
        I, J, IStart, IEnd : Integer;
        patch              : array (0 .. 4) of Integer;
        O                  : Order;
        Y                  : Exact_Subtyp;
      begin
        I := Locate_CD_Id (CD, block_data.context.level);
        if CD.id_table (I).entity = tache then
          In_Symbol;
          Entry_Call (CD, block_data.context, FSys_St, I, -1);
          if CD.ObjCode (CD.LC - 2).F = k_Call then  --  Need to patch CallType later
            patch (0) := CD.LC - 2;
          else
            patch (0) := CD.LC - 3;
          end if;       -- LC-1 must be OP=3, update Display
          patch (1) := CD.LC;  --  Need to patch in JMPC address later
          Emit_1 (CD, k_Jump_If_Zero_With_Pop, dummy_address_if);  --  JMPC, address patched in after ELSE
                                    --  or OR
          if CD.Sy = Semicolon then
            In_Symbol;
          else
            Error_then_Skip (CD, Semicolon, err_semicolon_missing);
          end if;
          if CD.Sy not in OR_Symbol | ELSE_Symbol then
            Sequence_of_Statements (CD, ELSE_OR, block_data);
          end if;
          if CD.Sy = OR_Symbol then  --  =====================> Timed Entry Call
            CD.ObjCode (patch (0)).X     := Timed_Entry_Call;
            CD.ObjCode (patch (0) + 1).Y := Timed_Entry_Call;  --  Exit type matches Entry type
            In_Symbol;
            if CD.Sy = DELAY_Symbol then
              In_Symbol;
              if CD.Sy = Semicolon then
                Select_Error (err_missing_expression_for_delay);
              else          --  Calculate delay value
                patch (2) := CD.LC;
                Expression (CD, block_data.context, Semicolon_Set, Y);
                patch (3) := CD.LC - 1;
                if Y.TYP /= Floats then
                  Select_Error (err_wrong_type_in_DELAY);
                else        --  End of timed Entry select ObjCode, do patching
                  CD.ObjCode (patch (1)).Y := Operand_2_Type (CD.LC);  --  if Entry not made, skip rest
                  J                      := patch (3) - patch (2) + 1;
                  IStart                 := patch (0);
                  IEnd                   := CD.LC - 1;
                  while J > 0 loop           --  Move delay time ObjCode To before
                    O := CD.ObjCode (IEnd);  --  opcodes k_Call, k_Return_Call.
                    for I in reverse IEnd - 1 .. IStart loop
                      CD.ObjCode (I + 1) := CD.ObjCode (I);
                    end loop;
                    CD.ObjCode (IStart) := O;
                    J                   := J - 1;
                  end loop;
                  In_Symbol;
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
            In_Symbol;
            Sequence_of_Statements (CD, END_Set, block_data);
            CD.ObjCode (patch (1)).Y  := Operand_2_Type (patch (3));
            CD.ObjCode (patch (2)).Y  := Operand_2_Type (CD.LC);
          end if;
          if CD.Sy /= END_Symbol then
            Select_Error (err_END_missing);
          end if;
          In_Symbol;
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
                In_Symbol;
              end loop;
            end if;        -- of parameters.
            if CD.Sy = RParent then
              In_Symbol;
            end if;
          end Accept_Call_2;

          I : Integer;
        begin         -- Accept_Statment_2
          In_Symbol;
          I := Locate_CD_Id (CD, block_data.context.level);
          if CD.id_table (I).entity /= entree then
            Select_Error (err_general_error);
          else
            CD.target.Mark_Reference (I);
          end if;
          In_Symbol;
          Accept_Call_2;
          Emit_2 (CD, k_Selective_Wait, 2, Operand_2_Type (I));      --  Retain Entry Index
          Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
          Emit_2 (CD, k_Selective_Wait, 3, Operand_2_Type (CD.LC));  --  ACCEPT IF Ready ELSE Skip To LC
          --  CONDITIONAL ACCEPT MUST BE ATOMIC
          if CD.Sy = DO_Symbol then
            if block_data.context.level = nesting_level_max then
              Fatal (LEVELS);  --  Exception is raised there.
            end if;
            block_data.context.level := block_data.context.level + 1;
            CD.Display (block_data.context.level) := CD.id_table (I).block_or_pkg_ref;
            In_Symbol;
            Sequence_of_Statements (CD, END_Set, block_data);
            Need_END_Symbol (CD);
            if CD.Sy = IDent then
              if CD.Id /= CD.id_table (I).name then
                Select_Error (err_incorrect_name_after_END);
              else
                CD.target.Mark_Reference (I);
              end if;
            end if;
            block_data.context.level := block_data.context.level - 1;
            In_Symbol;
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
              In_Symbol;  --  Consume WHEN symbol.
              Boolean_Expression (CD, block_data.context, FSys_St + Finger, X);
              In_Symbol;
              case CD.Sy is
                when ACCEPT_Symbol =>
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump_If_Zero_With_Pop);
                  Accept_Statement_2;
                when DELAY_Symbol =>
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump_If_Zero_With_Pop);
                  In_Symbol;
                  Expression (CD, block_data.context, FSys_St + Semicolon, Y);
                  Emit_2 (CD, k_Selective_Wait, 4, Operand_2_Type (CD.LC + 2));  --  Update delay time
                  if Y.TYP /= Floats then
                    Select_Error (err_wrong_type_in_DELAY);
                  end if;
                  Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                  Emit (CD, k_Jump);
                when others =>
                  Select_Error (err_missing_a_procedure_declaration);  --  ??
              end case;
              In_Symbol;
              Sequence_of_Statements (CD, ELSE_END_OR, block_data, True);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);          --  patch JMP ADDRESS AT EndSy
            --  end WHEN_Symbol

            when ACCEPT_Symbol =>
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              Accept_Statement_2;
              In_Symbol;
              Sequence_of_Statements (CD, ELSE_END_OR, block_data, True);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);

            when OR_Symbol =>
              In_Symbol;  --  Consume OR symbol.

            when ELSE_Symbol =>
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              In_Symbol;
              Sequence_of_Statements (CD, END_Set, block_data);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);

            when DELAY_Symbol =>
              Patch_Addresses (CD.ObjCode (CD.ObjCode'First .. CD.LC), Alt_Patch, IAlt);
              --  Generate a Task delay, calculate return value if req'D
              In_Symbol;
              if CD.Sy = Semicolon then
                Error_then_Skip (CD, Semicolon, err_missing_expression_for_delay);
              else          -- calculate return value
                Expression (CD, block_data.context, Semicolon_Set, Y);
                Emit_2 (CD, k_Selective_Wait, 4, Operand_2_Type (CD.LC + 2));  --  Update delay time
                if Y.TYP /= Floats then
                  Select_Error (err_wrong_type_in_DELAY);
                end if;
                Feed_Patch_Table (Alt_Patch, IAlt, CD.LC);
                Emit (CD, k_Jump);
              end if;
              In_Symbol;
              Sequence_of_Statements (CD, ELSE_END_OR, block_data, True);
              Feed_Patch_Table (JSD, ISD, CD.LC);
              Emit (CD, k_Jump);

            when TERMINATE_Symbol =>
              In_Symbol;
              if CD.Sy /= Semicolon then
                Select_Error (err_semicolon_missing);
              end if;
              do_terminate := True;        -- Oguz
              In_Symbol;

            when END_Symbol =>
              In_Symbol;
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
      In_Symbol;  --  Consume SELECT symbol.
      case CD.Sy is
        when ACCEPT_Symbol | WHEN_Symbol =>
          Selective_Wait;
          In_Symbol;
        when IDent =>  --  Task Entry objectName.
          Qualified_Entry_Call;
          In_Symbol;
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
        hint_1 => "Block statements don't work yet",
        severity => major
      );
      --
      block_statement_data.context.level                 := block_data.context.level + 1;
      block_statement_data.block_id_index                := CD.Id_Count;
      block_statement_data.entity                        := block_data.entity;
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
      --  Block_Statement or loop, named by a label, like this:
      --    "name : loop".
      --  We remember the identifier for checking purposes.
      new_ident_for_statement           : constant Alfa := CD.Id;
      new_ident_for_statement_with_case : constant Alfa := CD.Id_with_case;
      --
      procedure Check_ID_after_END_LOOP is  --  RM 5.5 (5)
        procedure Boom (err : Compile_Diagnostic) is
        begin
          Error (CD, err, A2S (new_ident_for_statement_with_case));
        end Boom;
      begin
        if CD.Sy = IDent then
          if CD.Id /= new_ident_for_statement then
            Boom (err_END_LOOP_ident_wrong);
          end if;
          In_Symbol;  --  Consume identifier.
        else
          Boom (err_END_LOOP_ident_missing);
        end if;
      end Check_ID_after_END_LOOP;
      --
      dummy_idx : Natural;
    begin
      Enter_Simple
        (CD,
         block_data.context.level,
         new_ident_for_statement,
         new_ident_for_statement_with_case,
         loop_identifier,
         dummy_idx);
      if CD.Sy /= Colon then
        Error
          (CD,
           err_colon_missing_for_named_statement,
           A2S (new_ident_for_statement_with_case),
           severity => major);
      end if;
      In_Symbol;  --  Consume ':' symbol.
      case CD.Sy is
        when BEGIN_Symbol | DECLARE_Symbol => -- Named Block_Statement
          Block_Statement (new_ident_for_statement);
        when FOR_Symbol =>
          FOR_Statement (CD.Id_Count);
          Check_ID_after_END_LOOP;
        when LOOP_Symbol =>
          LOOP_Statement (k_Jump, CD.LC, (CD.Id_Count, False, CD.CUD.location.line));
          Check_ID_after_END_LOOP;
        when WHILE_Symbol =>
          WHILE_Statement (CD.Id_Count);
          Check_ID_after_END_LOOP;
        when others =>
          Error (CD, err_general_error);
      end case;
    end Named_Statement;

    procedure Statement_starting_with_an_Identifier is
      I_Statement : constant Integer :=
        Locate_CD_Id (CD, block_data.context.level, Fail_when_No_Id => False);
    begin
      In_Symbol;
      if I_Statement = No_Id then
        --  Unknown identifier: must be an identifier for a named Block_Statement or loop.
        Named_Statement;
      else
        case CD.id_table (I_Statement).entity is
          when Object_Kind =>
            Assignment (CD, FSys_St, block_data.context, I_Statement, check_is_variable => True);
            Elevate_to_Maybe_or_Yes
              (CD.id_table (I_Statement).is_written_after_init, block_data.context);

          when declared_number_or_enum_item =>
            Error (CD, err_illegal_statement_start_symbol, "constant or an enumeration item",
                   severity => major);

          when type_mark =>
            Error (CD, err_illegal_statement_start_symbol, "type name", severity => major);

          when funktion | funktion_intrinsic =>
            Error (CD, err_illegal_statement_start_symbol, "function name", severity => major);

          when tache =>
            Entry_Call (CD, block_data.context, FSys_St, I_Statement, Normal_Entry_Call);

          when prozedure =>
            Subprogram_or_Entry_Call
              (CD, block_data.context, FSys_St, I_Statement, Normal_Procedure_Call);

          when prozedure_intrinsic =>
            Standard_Procedures.Standard_Procedure
              (CD, block_data.context, FSys_St, SP_Code'Val (CD.id_table (I_Statement).adr_or_sz));

          when loop_identifier =>
            Error (CD, err_duplicate_loop_identifier, A2S (CD.Id), severity => major);

          when paquetage =>
            Error (CD, err_illegal_statement_start_symbol, "package name", severity => major);

          when others =>
            Error
              (CD, err_illegal_statement_start_symbol,
               ". Entity found: " &
               Entity_Kind'Image (CD.id_table (I_Statement).entity), severity => major);
        end case;
      end if;
    end Statement_starting_with_an_Identifier;

    procedure Process_Name_for_Unnamed_Loop is
    begin
      if CD.Sy = IDent then
        Error
          (CD, err_general_error,
           "loop starting at line" &
           CD.Nested_Loop_Table (CD.loop_nesting_level + 1).start_line'Image &
           " has no name");
        In_Symbol;  --  Recovery.
      end if;
    end Process_Name_for_Unnamed_Loop;

  begin  --  Statement
    if CD.error_count > 0 then
      return;
    end if;
    if Statement_Begin_Symbol (CD.Sy) then
      case CD.Sy is
        when IDent =>
          Statement_starting_with_an_Identifier;
        when ACCEPT_Symbol =>
          Accept_Statement;
        when BEGIN_Symbol | DECLARE_Symbol =>
          --  Anonymous Block Statement
          Block_Statement (Empty_Alfa);
        when CASE_Symbol =>
          CASE_Statement (CD, FSys_St, block_data);
        when DELAY_Symbol =>
          Delay_Statement;
        when EXIT_Symbol =>
          Exit_Statement;
        when FOR_Symbol =>
          FOR_Statement (No_Id);
          Process_Name_for_Unnamed_Loop;
        when IF_Symbol =>
          IF_Statement;
        when LOOP_Symbol =>
          LOOP_Statement (k_Jump, CD.LC, (No_Id, False, CD.CUD.location.line));
          Process_Name_for_Unnamed_Loop;
        when NULL_Symbol =>
          In_Symbol;  --  Just consume the NULL symbol.
        when RETURN_Symbol =>
          RETURN_Statement;
        when SELECT_Symbol =>
          Select_Statement;
        when WHILE_Symbol =>
          WHILE_Statement (No_Id);
          Process_Name_for_Unnamed_Loop;
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
