with HAC_Sys.PCode.Interpreter.Tasking;

with Ada.Calendar;

package body HAC_Sys.PCode.Interpreter.Calls is

  procedure Do_Calling_Operation (
    CD :        Co_Defs.Compiler_Data;
    ND : in out In_Defs.Interpreter_Data
  )
  is
    use Defs, In_Defs;
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    use type HAC_Integer;

    procedure Do_Mark_Stack is
      VSize : constant Integer :=
        Integer (CD.Blocks_Table (CD.IdTab (Integer (IR.Y)).Block_Ref).VSize);
    begin
      if Curr_TCB.T + VSize > Curr_TCB.STACKSIZE then
        raise VM_Stack_Overflow;
      else
        Curr_TCB.T := Curr_TCB.T + 5;          --  Make room for fixed area
        ND.S (Curr_TCB.T - 1).I := HAC_Integer (VSize - 1);
        ND.S (Curr_TCB.T).I     := IR.Y;       --  CD.IdTab index of called procedure/entry
      end if;
    end Do_Mark_Stack;

    procedure Do_Call is
      use Ada.Calendar;
      F1 : HAC_Float;    --  Internal float registers
      H1, H2, H4 : Index;
      H3 : Nesting_level;
      H5 : Integer;
    begin
      --  procedure and task entry CALL
      --  Cramer
      if IR.X = Defs.Timed_Entry_Call then
        --  Timed entry call
        F1 := ND.S (Curr_TCB.T).R;  --  Pop delay time
        Pop (ND);
      end if;
      H1 := Curr_TCB.T - Integer (IR.Y);     --  base of activation record
      H2 := Index (ND.S (H1 + 4).I);         --  CD.IdTab index of called procedure/entry
      H3 := CD.IdTab (Integer (H2)).LEV;
      Curr_TCB.DISPLAY (H3 + 1) := Integer (H1);
      ND.S (H1 + 1).I := HAC_Integer (Curr_TCB.PC);  --  return address
      H4 := Index (ND.S (H1 + 3).I) + H1;  --  new top of stack
      ND.S (H1 + 2).I := HAC_Integer (Curr_TCB.DISPLAY (H3));  --  static link
      ND.S (H1 + 3).I := HAC_Integer (Curr_TCB.B);             --  dynamic link
      Curr_TCB.B := H1;
      Curr_TCB.T := H4;
      case IR.X is  --  Call type
        when Defs.Normal_Procedure_Call =>
          Curr_TCB.PC := CD.IdTab (H2).Adr_or_Sz;

        when Defs.Normal_Entry_Call =>
          Tasking.Queue (CD, ND, H2, ND.CurTask);  --  put self on entry queue
          Curr_TCB.TS := WaitRendzv;
          H5          := CD.IdTab (H2).Adr_or_Sz;  --  Task being entered
          if ((ND.TCB (H5).TS = WaitRendzv) and (ND.TCB (H5).SUSPEND = H2)) or
             (ND.TCB (H5).TS = TimedWait)
          then
            --  wake accepting task if necessary
            ND.TCB (H5).TS      := Ready;
            ND.TCB (H5).SUSPEND := 0;
          end if;
          ND.SWITCH := True;                 --  give up control

        when Defs.Timed_Entry_Call =>
          Tasking.Queue (CD, ND, H2, ND.CurTask);  --  put self on entry queue
          H5 := CD.IdTab (H2).Adr_or_Sz;  --  Task being entered
          --
          if ((ND.TCB (H5).TS = WaitRendzv) and (ND.TCB (H5).SUSPEND = H2)) or
             (ND.TCB (H5).TS = TimedWait)
          then
            --  wake accepting task if necessary
            Curr_TCB.TS := WaitRendzv;     --  suspend self
            ND.TCB (H5).TS := Ready;       --  wake accepting task
            ND.TCB (H5).SUSPEND := 0;
          else
            Curr_TCB.TS := TimedRendz;          --  Timed Wait For Rendezvous
            Curr_TCB.R1.I := 1;                 --  Init R1 to specify NO timeout
            Curr_TCB.R2.I := HAC_Integer (H2);  --  Save address of queue for purge
            ND.SYSCLOCK := Clock;               --  update System Clock
            Curr_TCB.WAKETIME := ND.SYSCLOCK + Duration (F1);
          end if;
          ND.SWITCH := True;       --  give up control

        when Defs.Conditional_Entry_Call =>
          H5 := CD.IdTab (H2).Adr_or_Sz;              --  Task being entered
          if ((ND.TCB (H5).TS = WaitRendzv) and (ND.TCB (H5).SUSPEND = H2)) or
             (ND.TCB (H5).TS = TimedWait)
          then
            Tasking.Queue (CD, ND, H2, ND.CurTask);  --  put self on entry queue
            Curr_TCB.R1.I := 1;        --  Indicate entry successful
            Curr_TCB.TS := WaitRendzv;
            ND.TCB (H5).TS      := Ready;  --  wake accepting task if required
            ND.TCB (H5).SUSPEND := 0;
            ND.SWITCH              := True;   --  give up control
          else
            --  can't wait, forget about entry call
            Curr_TCB.R1.I := 0;   --  Indicate entry failed in R1 1
            --  failure will be acknowledged by next instruction, 32
          end if;
        when others =>
          null;  -- [P2Ada]: no otherwise / else in Pascal
      end case;
    end Do_Call;

    procedure Do_Exit_Call is
    begin
      --  EXIT entry call or procedure call
      --  Cramer
      Curr_TCB.T := Curr_TCB.B - 1;
      if IR.Y = Defs.Normal_Procedure_Call then
        Curr_TCB.PC := Integer (ND.S (Curr_TCB.B + 1).I);  --  Standard proc call return
      end if;
      if Curr_TCB.PC /= 0 then
        Curr_TCB.B := Integer (ND.S (Curr_TCB.B + 3).I);
        if IR.Y = Defs.Timed_Entry_Call or IR.Y = Defs.Conditional_Entry_Call then
          if IR.Y = Defs.Timed_Entry_Call and Curr_TCB.R1.I = 0 then
            Push (ND);
          end if;
          --  A JMPC instruction always follows (?)
          --  timed and conditional entry call
          --  returns (32).  Push entry call
          ND.S (Curr_TCB.T).I := Curr_TCB.R1.I;    --  success indicator for JMPC.
        end if;
      else
        ND.TActive  := ND.TActive - 1;
        Curr_TCB.TS := Completed;
        ND.SWITCH   := True;
      end if;
    end Do_Exit_Call;

    procedure Do_Exit_Function is
    begin
      Curr_TCB.T  := Curr_TCB.B;
      Curr_TCB.PC := Integer (ND.S (Curr_TCB.B + 1).I);
      Curr_TCB.B  := Integer (ND.S (Curr_TCB.B + 3).I);
      if IR.Y = Defs.End_Function_without_Return and then ND.PS /= Exception_Raised then
        raise VM_Function_End_without_Return;
      end if;
    end Do_Exit_Function;

  begin
    case Calling_Opcode (ND.IR.F) is
      when k_Mark_Stack    => Do_Mark_Stack;
      when k_Call          => Do_Call;
      when k_Exit_Call     => Do_Exit_Call;
      when k_Exit_Function => Do_Exit_Function;
    end case;
  end Do_Calling_Operation;

end HAC_Sys.PCode.Interpreter.Calls;
