package body HAC.PCode.Interpreter.Multi_Statement is

  procedure Do_Multi_Statement_Operation (CD : Compiler_Data; ND : in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    H1, H2 : Defs.HAC_Integer;  --  Internal integer registers

    procedure Do_CASE_Switch_1 is
    begin
      H1 := ND.S (Curr_TCB.T).I;
      Pop (ND);
      H2 := IR.Y;
      --
      --  Now we loop over a bunch of k_CASE_Switch_2 instruction pairs that covers all cases.
      --
      loop
        if CD.ObjCode (H2).F /= k_CASE_Switch_2 then
          ND.PS := Case_Check_Error;  --  Value or OTHERS not found. This situation should not...
          exit;                       --  ...happen: compiler should check it before run-time.
        elsif CD.ObjCode (H2).Y = H1    --  either: - value is matching
              or CD.ObjCode (H2).X < 0  --      or: - "WHEN OTHERS =>" case
        then
          Curr_TCB.PC := CD.ObjCode (H2 + 1).Y;  --  Execute instructions after "=>".
          exit;
        else
          H2 := H2 + 2;  --  Check the next k_CASE_Switch_2 instruction pair.
        end if;
      end loop;
    end Do_CASE_Switch_1;

    procedure Do_CASE_Switch_2 is
    --  This instruction appears only in a special object code block, see k_CASE_Switch_1.
    begin
      null;
    end;

    procedure Do_FOR_Forward_Begin is  --  Start of a FOR loop, forward direction
    begin
      H1 := ND.S (Curr_TCB.T - 1).I;
      if H1 <= ND.S (Curr_TCB.T).I then
        ND.S (ND.S (Curr_TCB.T - 2).I).I := H1;
      else
        Curr_TCB.T  := Curr_TCB.T - 3;
        Curr_TCB.PC := IR.Y;
      end if;
    end Do_FOR_Forward_Begin;

    procedure Do_FOR_Forward_End is  --  End of a FOR loop, forward direction
    begin
      H2 := ND.S (Curr_TCB.T - 2).I;
      H1 := ND.S (H2).I + 1;
      if H1 <= ND.S (Curr_TCB.T).I then
        ND.S (H2).I    := H1;
        Curr_TCB.PC := IR.Y;
      else
        Pop (ND, 3);
      end if;
    end Do_FOR_Forward_End;

    procedure Do_FOR_Reverse_Begin is  --  Start of a FOR loop, reverse direction
    begin
      H1 := ND.S (Curr_TCB.T).I;
      if H1 >= ND.S (Curr_TCB.T - 1).I then
        ND.S (ND.S (Curr_TCB.T - 2).I).I := H1;
      else
        Curr_TCB.PC := IR.Y;
        Curr_TCB.T  := Curr_TCB.T - 3;
      end if;
    end Do_FOR_Reverse_Begin;

    procedure Do_FOR_Reverse_End is  --  End of a FOR loop, reverse direction
    begin
      H2 := ND.S (Curr_TCB.T - 2).I;
      H1 := ND.S (H2).I - 1;
      if H1 >= ND.S (Curr_TCB.T - 1).I then
        ND.S (H2).I := H1;
        Curr_TCB.PC := IR.Y;
      else
        Pop (ND, 3);
      end if;
    end Do_FOR_Reverse_End;

  begin
    case Multi_Statement_Opcode (ND.IR.F) is
      when k_CASE_Switch_1      => Do_CASE_Switch_1;
      when k_CASE_Switch_2      => Do_CASE_Switch_2;
      when k_FOR_Forward_Begin  => Do_FOR_Forward_Begin;
      when k_FOR_Forward_End    => Do_FOR_Forward_End;
      when k_FOR_Reverse_Begin  => Do_FOR_Reverse_Begin;
      when k_FOR_Reverse_End    => Do_FOR_Reverse_End;
    end case;
  end Do_Multi_Statement_Operation;

end HAC.PCode.Interpreter.Multi_Statement;
