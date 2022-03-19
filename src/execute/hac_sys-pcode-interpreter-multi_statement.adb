package body HAC_Sys.PCode.Interpreter.Multi_Statement is

  procedure Do_Multi_Statement_Operation (CD : Compiler_Data; ND : in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    use type Defs.HAC_Integer;

    procedure Do_CASE_Switch_1 is
      Value : constant Defs.HAC_Integer := ND.S (Curr_TCB.T).I;
      H2 : Integer;
      jump : Boolean;
    begin
      Pop (ND);
      H2 := Integer (IR.Y);
      --
      --  Now we loop over a bunch of (k_CASE_Choice_Data, k_CASE_Match_Jump) pairs
      --  that should covers all cases.
      --
      loop
        if CD.ObjCode (H2).F not in CASE_Any_Choice then
          --  We hit the end of (k_CASE_Choice_Data, k_CASE_Match_Jump) pairs.
          --  This means that Value, or OTHERS, were not found so far.
          --  This situation should not happen; it should be caught at compile-time.
          raise VM_Case_Check_Error;
        end if;
        case CASE_Any_Choice (CD.ObjCode (H2).F) is
          when k_CASE_Choice_Value => jump := Value = CD.ObjCode (H2).Y;
          when k_CASE_Choice_Range => jump := Value in CD.ObjCode (H2).X .. CD.ObjCode (H2).Y;
          when k_CASE_Choice_Others => jump := True;
        end case;
        if jump then
          --  The interpreter will execute instructions following "=>".
          --  The address is stored with a k_CASE_Match_Jump instruction just after
          --  the CASE_Any_Choice instruction.
          Curr_TCB.PC := Defs.Index (CD.ObjCode (H2 + 1).Y);
          exit;
        end if;
        --  Check the next (CASE_Any_Choice, k_CASE_Match_Jump) instruction pair:
        H2 := H2 + 2;
      end loop;
    end Do_CASE_Switch_1;

    procedure Do_FOR_Forward_Begin is  --  Start of a FOR loop, forward direction
      FOR_Param_Addr : constant Defs.Index       := Defs.Index (ND.S (Curr_TCB.T - 2).I);
      Lower_Bound    : constant Defs.HAC_Integer := ND.S (Curr_TCB.T - 1).I;
      Upper_Bound    : constant Defs.HAC_Integer := ND.S (Curr_TCB.T).I;
    begin
      if Lower_Bound <= Upper_Bound then
        --  We can start the loop with the first value
        ND.S (FOR_Param_Addr).I := Lower_Bound;
      else
        --  Empty range -> we don't enter the loop at all -> Jump after loop's end.
        Curr_TCB.PC := Defs.Index (IR.Y);
      end if;
    end Do_FOR_Forward_Begin;

    procedure Do_FOR_Forward_End is  --  End of a FOR loop, forward direction
      FOR_Param_Addr : constant Defs.Index       := Defs.Index (ND.S (Curr_TCB.T - 2).I);
      Upper_Bound    : constant Defs.HAC_Integer := ND.S (Curr_TCB.T).I;
      Next_Value : Defs.HAC_Integer;
    begin
      Next_Value := ND.S (FOR_Param_Addr).I + 1;  --  !! Overflow check before here
      if Next_Value <= Upper_Bound then
        ND.S (FOR_Param_Addr).I := Next_Value;
        Curr_TCB.PC := Defs.Index (IR.Y);  --  Jump back to loop's begin
      else
        null;  --  Leave loop (just go to next instruction, k_FOR_Release_Stack_After_End)
      end if;
    end Do_FOR_Forward_End;

    procedure Do_FOR_Reverse_Begin is  --  Start of a FOR loop, reverse direction
      FOR_Param_Addr : constant Defs.Index       := Defs.Index (ND.S (Curr_TCB.T - 2).I);
      Lower_Bound    : constant Defs.HAC_Integer := ND.S (Curr_TCB.T - 1).I;
      Upper_Bound    : constant Defs.HAC_Integer := ND.S (Curr_TCB.T).I;
    begin
      if Lower_Bound <= Upper_Bound then
        --  We can start the loop with the first value
        ND.S (FOR_Param_Addr).I := Upper_Bound;
      else
        --  Empty range -> we don't enter the loop at all -> Jump after loop's end.
        Curr_TCB.PC := Defs.Index (IR.Y);
      end if;
    end Do_FOR_Reverse_Begin;

    procedure Do_FOR_Reverse_End is  --  End of a FOR loop, reverse direction
      FOR_Param_Addr : constant Defs.Index       := Defs.Index (ND.S (Curr_TCB.T - 2).I);
      Lower_Bound    : constant Defs.HAC_Integer := ND.S (Curr_TCB.T - 1).I;
      Next_Value : Defs.HAC_Integer;
    begin
      Next_Value := ND.S (FOR_Param_Addr).I - 1;  --  !! Overflow check before here
      if Next_Value >= Lower_Bound then
        ND.S (FOR_Param_Addr).I := Next_Value;
        Curr_TCB.PC := Defs.Index (IR.Y);  --  Jump back to loop's begin
      else
        null;  --  Leave loop (just go to next instruction, k_FOR_Release_Stack_After_End)
      end if;
    end Do_FOR_Reverse_End;

  begin
    case Multi_Statement_Opcode (ND.IR.F) is
      when k_CASE_Switch        => Do_CASE_Switch_1;
      when CASE_Data_Opcode     => null;  --  Only via k_CASE_Switch.
      when k_FOR_Forward_Begin  => Do_FOR_Forward_Begin;
      when k_FOR_Forward_End    => Do_FOR_Forward_End;
      when k_FOR_Reverse_Begin  => Do_FOR_Reverse_Begin;
      when k_FOR_Reverse_End    => Do_FOR_Reverse_End;
      when k_FOR_Release_Stack_After_End  =>
        Pop (ND, 3);  --  Destack parameter and bounds that are used by the FOR loop.
    end case;
  end Do_Multi_Statement_Operation;

end HAC_Sys.PCode.Interpreter.Multi_Statement;
