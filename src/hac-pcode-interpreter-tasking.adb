with Ada.Calendar;

package body HAC.PCode.Interpreter.Tasking is

  function EIndex (CD : Compiler_Data; Entry_Index : Integer) return Integer is
    i, e : Integer;
  begin
    e := -1;
    i := 1;
    while i <= CD.Entries_Count and e = -1 loop
      if Entry_Index = CD.Entries_Table (i) then
        e := i;
      end if;
      i := i + 1;
    end loop;
    return e;
  end EIndex;

  function FirstCaller (CD : Compiler_Data; ND: in out Interpreter_Data; Entry_Index : Integer) return Integer is
    ix, val : Integer;
  begin
    ix := EIndex (CD, Entry_Index);
    if ND.EList (ix).First = null then
      val := -1;
    else
      val := ND.EList (ix).First.Task_Index;
    end if;
    return val;
  end FirstCaller;

  use Ada.Calendar;

  procedure Do_Accept_Rendezvous (CD : Compiler_Data; ND: in out Interpreter_Data) is
    --  Hathorn, Cramer
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    H1, H2, H3 : Defs.HAC_Integer;  --  Internal integer registers
  begin
    H1 := IR.Y;                         --  entry pointer
    H2 := FirstCaller (CD, ND, H1);     --  first waiting task
    H3 := Integer (CD.IdTab (H1).LEV);  --  level of accepting entry
    if H2 >= 0 then
      --  start rendzv if call is waiting
      Curr_TCB.DISPLAY (Nesting_level (H3 + 1)) := ND.TCB (H2).B; --  address callers
      --  parms
      Curr_TCB.InRendzv := H2;  --  indicate that task is in Rendzv
      if ND.TCB (H2).TS = TimedRendz then
        ND.TCB (H2).TS := WaitRendzv;
      end if;
    else
      --  or put self to sleep
      Curr_TCB.SUSPEND := H1;
      Curr_TCB.TS      := WaitRendzv;      --  status is waiting for
      --rendezvous
      Curr_TCB.PC      := Curr_TCB.PC - 1;          --  do this
      --step again when awakened

    end if;
    ND.SWITCH := True;
  end Do_Accept_Rendezvous;

  procedure Do_Selective_Wait (CD : Compiler_Data; ND: in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    use type Defs.HAC_Float;
    H1, H2, H3 : Defs.HAC_Integer;  --  Internal integer registers
  begin
    case IR.X is
      when 1 => --  Start Selective Wait seq.
        Curr_TCB.R1.I := 0; --  next instruction if delay expires
        Curr_TCB.R2.R := -1.0; --  delay time

      when 2 => --  Retain entry ID
        Curr_TCB.R3.I := IR.Y;

      when 3 => --  Accept if its still on queue
        H1 := Curr_TCB.R3.I;
        H2 := FirstCaller (CD, ND, H1);    --  first waiting task
        H3 := Defs.HAC_Integer (CD.IdTab (H1).LEV);     --  level of accepting entry
        if H2 >= 0 then
          Curr_TCB.DISPLAY (Nesting_level (H3 + 1)) := ND.TCB (H2).B;
            --  address callers parms
          Curr_TCB.InRendzv := H2;             --  indicate task InRendz
          if ND.TCB (H2).TS = TimedRendz then  --  turn off entry timeout
            ND.TCB (H2).TS := WaitRendzv;      --  if it was on
          end if;
        else
          Curr_TCB.PC := IR.Y; --  Jump to patched in address
        end if;
        ND.SWITCH := True;

      when 4 => --  Update minimum delay time
        if ND.S (Curr_TCB.T).R > 0.0 then
          if Curr_TCB.R2.R = -1.0 then
            Curr_TCB.R2.R := ND.S (Curr_TCB.T).R;
            Curr_TCB.R1.I := IR.Y;   --  ins after JMP
          else
            if ND.S (Curr_TCB.T).R < Curr_TCB.R2.R then
              Curr_TCB.R2.R := ND.S (Curr_TCB.T).R;
              Curr_TCB.R1.I := IR.Y;   --  ins after JMP
            end if;
          end if;
        end if;
        Pop (ND);

      when 5 | 6 => --  end of SELECT

        if Curr_TCB.R2.R > 0.0 then
          --  Timed Wait
          Curr_TCB.TS       := TimedWait;
          ND.SYSCLOCK          := GetClock;
          Curr_TCB.WAKETIME := ND.SYSCLOCK + Duration (Curr_TCB.R2.R);
          Curr_TCB.PC       := IR.Y; --  Do SELECT again when awakened by caller
          ND.SWITCH := True;  --  give up control
        end if;
        --  AVL -- TERMINATE
        --  IS THE PARENT TASK COMPLETED?
        if ND.TCB (0).TS = Completed and ND.CurTask /= 0 and IR.X /= 6 then
          ND.Nb_Callers := 0; --  LET'S SEE IF THERE ARE CALLERS
          for ITERM in 1 .. CD.Entries_Count loop
            if ND.EList (ITERM).First /= null then
              ND.Nb_Callers := ND.Nb_Callers + 1;
            end if;
          end loop;
          --  YES, NO CALLERS
          if ND.Nb_Callers = 0 then  --  YES, NO CALLERS
            --  ARE THE SIBLING TASKS EITHER COMPLETED OR
            --  IN THE SAME STATE AS CURTASK?
            ND.Nb_Complete := 0;
            for ITERM in 1 .. CD.Tasks_Definitions_Count loop
              if ND.TCB (ITERM).TS = Completed then
                ND.Nb_Complete := ND.Nb_Complete + 1;
              else
                if ND.TCB (ITERM).TS = Curr_TCB.TS then
                  ND.Nb_Complete := ND.Nb_Complete + 1;
                else
                  if ND.TCB (ITERM).TS = Ready and
                     Curr_TCB.TS = Running
                  then
                    ND.Nb_Complete := ND.Nb_Complete + 1;
                  end if;
                end if;
              end if;
            end loop;
            if CD.Tasks_Definitions_Count = ND.Nb_Complete then
              --  YES, THEN ALL TASKS ARE NOW TERMINATING
              for ITERM in 1 .. CD.Tasks_Definitions_Count loop
                ND.TCB (ITERM).TS := Terminated;
              end loop;
              ND.PS := FIN;
            end if;
          end if;
        end if;
      --               if ir.x = 6 then
      --               begin
      --                 term := false ;    {Task doesn't have a terminate}
      --               end ;                {alternative}
      --

      when others =>
        null;  -- [P2Ada]: no otherwise / else in Pascal
    end case;
  end Do_Selective_Wait;

end HAC.PCode.Interpreter.Tasking;
