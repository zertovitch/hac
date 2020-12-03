package body HAC_Sys.PCode.Interpreter.Multi_Statement is

  procedure Do_Multi_Statement_Operation (CD : Compiler_Data; ND : in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    H1 : Defs.HAC_Integer;  --  Internal integer registers
    use type Defs.HAC_Integer;

    procedure Do_CASE_Switch_1 is
      Value : constant Defs.HAC_Integer := ND.S (Curr_TCB.T).I;
      H2 : Integer;
    begin
      Pop (ND);
      H2 := Integer (IR.Y);
      --
      --  Now we loop over a bunch of (k_CASE_Choice_Data, k_CASE_Match_Jump) pairs
      --  that should covers all cases.
      --
      loop
        if CD.ObjCode (H2).F /= k_CASE_Choice_Data then
          --  We hit the end of (k_CASE_Choice_Data, k_CASE_Match_Jump) pairs.
          --  This means that Value, or OTHERS, were not found so far.
          --  This situation should not happen; should be caught at compile-time.
          raise VM_Case_Check_Error;
        elsif CD.ObjCode (H2).Y = Value
              or CD.ObjCode (H2).X = Defs.Case_when_others
        then
          Curr_TCB.PC := Defs.Index (CD.ObjCode (H2 + 1).Y);
          --  The interpreter will execute instructions following "=>".
          --  The address is stored with a k_CASE_Match_Jump.
          exit;
        else
          --  Check the next (k_CASE_Choice_Data, k_CASE_Match_Jump) instruction pair:
          H2 := H2 + 2;
        end if;
      end loop;
    end Do_CASE_Switch_1;

    procedure Do_FOR_Forward_Begin is  --  Start of a FOR loop, forward direction
    begin
      H1 := ND.S (Curr_TCB.T - 1).I;
      if H1 <= ND.S (Curr_TCB.T).I then
        ND.S (Defs.Index (ND.S (Curr_TCB.T - 2).I)).I := H1;
      else
        Curr_TCB.T  := Curr_TCB.T - 3;
        Curr_TCB.PC := Defs.Index (IR.Y);
      end if;
    end Do_FOR_Forward_Begin;

    procedure Do_FOR_Forward_End is  --  End of a FOR loop, forward direction
      H2 : Defs.Index;  --  Address of the FOR parameter ("the variable").
    begin
      H2 := Defs.Index (ND.S (Curr_TCB.T - 2).I);
      H1 := ND.S (H2).I + 1;
      if H1 <= ND.S (Curr_TCB.T).I then
        ND.S (H2).I := H1;
        Curr_TCB.PC := Defs.Index (IR.Y);  --  Jump to loop's begin
      else
        Pop (ND, 3);  --  Leave loop
      end if;
    end Do_FOR_Forward_End;

    procedure Do_FOR_Reverse_Begin is  --  Start of a FOR loop, reverse direction
    begin
      H1 := ND.S (Curr_TCB.T).I;
      if H1 >= ND.S (Curr_TCB.T - 1).I then
        ND.S (Defs.Index (ND.S (Curr_TCB.T - 2).I)).I := H1;
      else
        Curr_TCB.PC := Defs.Index (IR.Y);
        Curr_TCB.T  := Curr_TCB.T - 3;
      end if;
    end Do_FOR_Reverse_Begin;

    procedure Do_FOR_Reverse_End is  --  End of a FOR loop, reverse direction
      H2 : Defs.Index;  --  Address of the FOR parameter ("the variable").
    begin
      H2 := Defs.Index (ND.S (Curr_TCB.T - 2).I);
      H1 := ND.S (H2).I - 1;
      if H1 >= ND.S (Curr_TCB.T - 1).I then
        ND.S (H2).I := H1;
        Curr_TCB.PC := Defs.Index (IR.Y);  --  Jump to loop's begin
      else
        Pop (ND, 3);  --  Leave loop
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
    end case;
  end Do_Multi_Statement_Operation;

end HAC_Sys.PCode.Interpreter.Multi_Statement;
