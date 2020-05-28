with Ada.Calendar;
with Ada.Numerics.Float_Random;

package body HAC.PCode.Interpreter.Tasking is

  function Any_Task_Delayed (CD : Compiler_Data; ND: Interpreter_Data) return Boolean is
    task_delayed : Boolean := False;
  begin
    for t in TRange'First .. CD.Tasks_Definitions_Count loop
      task_delayed := ND.TCB (t).TS = Delayed or
                     ND.TCB (t).TS = TimedRendz or
                     ND.TCB (t).TS = TimedWait;
      exit when task_delayed;
    end loop;
    return task_delayed;
  end Any_Task_Delayed;

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

  function First_Caller (CD : Compiler_Data; ND: in out Interpreter_Data; Entry_Index : Integer) return Integer is
    ix, val : Integer;
  begin
    ix := EIndex (CD, Entry_Index);
    if ND.EList (ix).First = null then
      val := -1;
    else
      val := ND.EList (ix).First.Task_Index;
    end if;
    return val;
  end First_Caller;

  procedure Queue (
    CD           :        Compiler_Data;
    ND           : in out Interpreter_Data;
    Entry_Index  :        Integer;
    Calling_Task :        TRange
  )
  is
    ix         : constant Integer := EIndex (CD, Entry_Index);
    enode_var  : constant Eptr := new Enode'(Task_Index => Calling_Task, Next => null);
    E_Q_Header : EHeader renames ND.EList (ix);
  begin
    --  Queue an entry call by Calling_Task for entry 'Entry'.
    if E_Q_Header.First = null then
      E_Q_Header.First := enode_var;
    else
      E_Q_Header.Last.Next := enode_var;
    end if;
    E_Q_Header.Last := enode_var;
  end Queue;

  use Ada.Calendar;

  procedure Do_Accept_Rendezvous (CD : Compiler_Data; ND: in out Interpreter_Data) is
    --  Hathorn, Cramer
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    H1, H2, H3 : Defs.HAC_Integer;  --  Internal integer registers
  begin
    H1 := IR.Y;                         --  entry pointer
    H2 := First_Caller (CD, ND, H1);     --  first waiting task
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

  procedure Do_End_Rendezvous (CD : Compiler_Data; ND: in out Interpreter_Data) is
    --  Hathorn
    function Remove_First (Entry_Index : Integer) return TRange is
      ix, val : Integer;
      dmy     : Eptr;
    begin
      ix := EIndex (CD, Entry_Index);
      declare
        E_Q_Header : EHeader renames ND.EList (ix);
      begin
        val := E_Q_Header.First.Task_Index;
        if E_Q_Header.First = E_Q_Header.Last then
          E_Q_Header.First := null;
          E_Q_Header.Last  := null;
        else
          dmy              := E_Q_Header.First;
          E_Q_Header.First := E_Q_Header.First.Next;
          Dispose (dmy);
        end if;
      end;
      return val;
    end Remove_First;
    --
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    H1, H2 : Defs.HAC_Integer;  --  Internal integer registers
  begin
    Curr_TCB.InRendzv := NilTask;  --  indicate rendezvous has ended
    H1 := ND.IR.Y;                   --  entry pointer
    H2 := Remove_First (H1);       --  waiting task pointer
    if H2 >= 0 then
      --  wake up waiting task
      ND.TCB (H2).SUSPEND := 0;
      ND.TCB (H2).TS      := Ready;
      ND.SWITCH              := True;
    end if;
  end Do_End_Rendezvous;

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
        H2 := First_Caller (CD, ND, H1);    --  first waiting task
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

  procedure Do_Signal_Semaphore (CD : Compiler_Data; ND: in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    H1 : constant Defs.HAC_Integer := ND.S (Curr_TCB.T).I;
    H2, H3 : Defs.HAC_Integer;
    use Ada.Numerics.Float_Random;
  begin
    Pop (ND);
    H2 := CD.Tasks_Definitions_Count + 1;
    H3 := Integer (Random (ND.Gen) * Float (H2));
    while H2 >= 0 and ND.TCB (H3).TS /= WaitSem and ND.TCB (H3).SUSPEND /= H1
    loop
      H3 := (H3 + 1) mod (Defs.TaskMax + 1);
      H2 := H2 - 1;
    end loop;
    if H2 < 0 or ND.S (H1).I < 0 then
      ND.S (H1).I := ND.S (H1).I + 1;
    else
      ND.TCB (H3).SUSPEND := 0;
      ND.TCB (H3).TS      := Ready;
    end if;
    Curr_TCB.TS := Ready; --  end critical section
    ND.SWITCH := True;
  end Do_Signal_Semaphore;

  procedure Do_Wait_Semaphore (ND: in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    H1 : constant Defs.HAC_Integer := ND.S (Curr_TCB.T).I;
  begin
    Pop (ND);
    if ND.S (H1).I > 0 then
      ND.S (H1).I       := ND.S (H1).I - 1;
      Curr_TCB.TS := Critical;   --  In a critical section, task gets
      --  exclusive access to the virtual
    else
      --  processor until section ends.
      Curr_TCB.SUSPEND := H1;
      Curr_TCB.TS      := WaitSem;
      ND.SWITCH           := True;
    end if;
  end Do_Wait_Semaphore;

    procedure Init_main_task (CD : Compiler_Data; ND: in out Interpreter_Data) is
      use Ada.Numerics.Float_Random;
    begin
      Reset (ND.Gen);  --  initialize pseudo-random number generator
      --  After compiled, just begin exec
      --  Initialize run-time stack
      ND.S (1).I := 0 ;
      ND.S (2).I := 0 ;
      ND.S (3).I := -1 ;
      ND.S (4).I := CD.Tasks_Definitions_Table (0) ;
      declare
        Main_TCB : Task_Control_Block renames ND.TCB (0);
      begin
        Main_TCB.PC := CD.IdTab (CD.Tasks_Definitions_Table (0)).Adr_or_Sz ; --  first pcode instruction
        Main_TCB.T := CD.Blocks_Table (1).VSize - 1 ; -- was CD.Blocks_Table (2)
        Main_TCB.B := 0 ;
        Main_TCB.TS := Ready ;
        Main_TCB.InRendzv := NilTask ;
        Main_TCB.DISPLAY (1) := 0 ;
        Main_TCB.STACKSIZE := Defs.StMax - (CD.Tasks_Definitions_Count * Defs.STKINCR) ;
        Main_TCB.SUSPEND := 0 ;
        Main_TCB.QUANTUM := TSlice;
        Main_TCB.Pcontrol.UPRI := 0 ;
        Main_TCB.Pcontrol.INHERIT := False ;
        Main_TCB.LASTRUN := ND.Start_Time ;
      end;
    end Init_main_task;

    procedure Init_other_tasks (CD : Compiler_Data; ND: in out Interpreter_Data) is
      H1 : Defs.HAC_Integer;
    begin
      for Task_To_Init in 1 .. CD.Tasks_Definitions_Count loop
        declare
          Curr_TCB : Task_Control_Block renames ND.TCB (Task_To_Init);
        begin
          H1 := CD.Tasks_Definitions_Table (Task_To_Init) ;
          Curr_TCB.PC := CD.IdTab (H1).Adr_or_Sz ;
          Curr_TCB.B := ND.TCB (Task_To_Init - 1).STACKSIZE + 1 ;
          Curr_TCB.T := Curr_TCB.B + CD.Blocks_Table (CD.IdTab (H1).Block_Ref).VSize - 1 ;
          ND.S (Curr_TCB.B + 1).I := 0 ;
          ND.S (Curr_TCB.B + 2).I := 0 ;
          ND.S (Curr_TCB.B + 3).I := -1 ;
          ND.S (Curr_TCB.B + 4).I := H1 ;
          Curr_TCB.DISPLAY (1) := 0 ;
          Curr_TCB.DISPLAY (2) := Curr_TCB.B ;
          Curr_TCB.STACKSIZE := Curr_TCB.B + Defs.STKINCR - 1 ;
          Curr_TCB.SUSPEND := 0 ;
          Curr_TCB.TS := Ready ;
          Curr_TCB.InRendzv := NilTask ;
          Curr_TCB.QUANTUM := TSlice;
          Curr_TCB.Pcontrol.UPRI := 0 ;
          Curr_TCB.Pcontrol.INHERIT := False ;
          Curr_TCB.LASTRUN := ND.Start_Time ;
        end;
      end loop;
      --  Initially no queued entry calls
      for E_Idx in 1 .. CD.Entries_Count loop
        ND.EList (E_Idx).Task_Index := CD.IdTab (CD.Entries_Table (E_Idx)).Adr_or_Sz ;  --  Task index
        ND.EList (E_Idx).First := null ;
        ND.EList (E_Idx).Last  := null ;
      end loop;
      ND.TActive := CD.Tasks_Definitions_Count ;  --  All tasks are active initially
      ND.CurTask := 0 ;  --  IT WAS -1 ?
      ND.SWITCH := True ;
      ND.TIMER := ND.Start_Time; -- was 0.0
      ND.PS := RUN ;
    end Init_other_tasks;

    procedure ShowQ (
      CD          : Compiler_Data;
      ND          : in out Interpreter_Data;
      Entry_Index : Integer
    )
    is
      ix : constant Integer := EIndex (CD, Entry_Index);
      p  : Eptr := ND.EList (ix).First;
      use Defs, Ada.Text_IO;
    begin
      Put ("Dumping q for entry " & To_String (CD.IdTab (Entry_Index).Name) & " entry index=");
      IIO.Put (ix);
      New_Line;
      if p = null then
        Put ("*** EMPTY ***");
        New_Line;
      else
        loop
          Put ("Task ");
          Put (To_String (CD.IdTab (CD.Tasks_Definitions_Table (p.Task_Index)).Name));
          New_Line;
          p := p.Next;
          exit when p = null;
        end loop;
      end if;
    end ShowQ;

  procedure Tasks_to_wake (
    CD     :        Compiler_Data;
    ND     : in out Interpreter_Data;
    Result :    out Boolean
  )
  is
    procedure Purge (Entry_Index : Integer; t : TRange) is
      p, q : Eptr;     --  has timed out, the entry
      ix : Integer;     --  is purged from the q.
    begin
      ix := EIndex (CD, Entry_Index);
      q  := null;
      p  := ND.EList (ix).First;
      while p /= null loop
        if p.Task_Index = t then
          if ND.EList (ix).First =ND. EList (ix).Last then
            ND.EList (ix).First := null;
            ND.EList (ix).Last  := null;
          else
            if p = ND.EList (ix).First then
              ND.EList (ix).First := p.Next;
            else
              if p = ND.EList (ix).Last then
                ND.EList (ix).Last := q;
                q.Next      := null;
              else
                q.Next := p.Next;
              end if;
            end if;
          end if;
          Dispose (p);
          p := null; --  to exit loop
        else
          --  try next entry in list
          q := p;
          p := p.Next;
        end if;
      end loop;
    end Purge;
    --
      count : Integer := 0;
    begin
      for t in 0 .. CD.Tasks_Definitions_Count loop
        if (ND.TCB (t).TS = Delayed or
            ND.TCB (t).TS = TimedRendz or
            ND.TCB (t).TS = TimedWait) and
           ND.SYSCLOCK >= ND.TCB (t).WAKETIME
        then
          if ND.TCB (t).TS = TimedRendz then
            ND.TCB (t).R1.I := 0; --  timeout on rendezvous
            Purge (ND.TCB (t).R2.I, t);  --  remove from callee's q
          end if;
          if ND.TCB (t).TS = TimedWait then
            ND.TCB (t).PC := ND.TCB (t).R1.I; --  t.out on accept
          end if;
          ND.TCB (t).TS := Ready;
          count := count + 1;
        end if;
      end loop;
      Result := count > 0;
    end Tasks_to_wake;

end HAC.PCode.Interpreter.Tasking;