--  Translated from newsched.pas on 28-Jun-2022 by (New) P2Ada v. 28-Oct-2009

separate (HAC_Sys.PCode.Interpreter.Tasking)
procedure Scheduler (CD : Compiler_Data; ND : in out Interpreter_Data) is

  TCount : constant Natural := CD.Tasks_Definitions_Count;

  --  =======================================================================
  --   Calc_Priority:  Returns dynamic priority of task t.
  --
  --     Dynamic priority is a function of:
  --       User specified task priority which can be dynamically set via
  --         SMALL_SP.PRIORITY.
  --
  --  9X   Priority inheritance - if a task is inheriting priorities then
  --         its dynamic priority is the maximum of the dynamic priorities
  --         of ALL tasks enqueued to rendezvous with it.  Inheritance is
  --         activated for a task by calling SMALL_SP.INHERITP(TRUE).
  --       Rendezvous status - if a task X is not inheriting priorities but
  --         is in rendezvous with another task Y then X's dynamic priority
  --         is the larger of X's priority and Y's dynamic priority.
  --
  --  =======================================================================
  --  Translated from interdef.pas on 28-Jun-2022 by (New) P2Ada v. 28-Oct-2009
  --
  function Calc_Priority (t : TRange) return Float is
    Result_Calc_Priority, p1, p2 : Float;
                              et : Eptr;
    --  Convert integer user priority to real range [0.0, 1.0]
    function  CVTR (upri : Integer) return Float is
    begin  --  upri is in range [0, PriMax]
      return Float (upri) / Float (Defs.PriMax);
    end CVTR;

    --  Scheduling fairness adjustment to be added to static priority
    function Fairness (t : TRange) return Float is
      use Ada.Calendar;
    begin  --  Adjustment is directly prop. to elapsed time since last run
      return
        Float (Clock - ND.TCB (t).LASTRUN) / 20000.0;  --  20000.0 for tuning
    end Fairness;

  begin
    if ND.TCB (t).Pcontrol.INHERIT then  --  Consider ALL tasks waiting on task t
      p1 := CVTR (ND.TCB (t).Pcontrol.UPRI) + Fairness (t);
      for e in 1 .. CD.Entries_Count loop  --  For each entry
        if ND.EList (e).Task_Index = t then
          --  If an entry queue for task t
          --  then check priority of every task on queue
          et := ND.EList (e).First;
          while et /= null loop
            --  Calc Priority, RECURSIVE
            p2 := Calc_Priority (et.all.Task_Index);
            if p2 > p1 then
              p1 := p2;  --  Keep maximum priority
            end if;
            et := et.all.Next;  --  Next task in queue
          end loop;
        end if;
      end loop;
      Result_Calc_Priority := p1;
    elsif ND.TCB (t).InRendzv /= NilTask then  --  In Rendezvous with some task
      --  Compare task t pri. with task in rendezvous with, use max.
      p1 := CVTR (ND.TCB (t).Pcontrol.UPRI) + Fairness (t);  --  task t
      p2 := Calc_Priority (ND.TCB (t).InRendzv);             --  task t's caller
      Result_Calc_Priority := Float'Max (p1, p2);
    else
      --  Not inheriting and not in rendezvous, simplest case.
      Result_Calc_Priority := CVTR (ND.TCB (t).Pcontrol.UPRI) + Fairness (t);
    end if;
    return Result_Calc_Priority;
  end Calc_Priority;

  --                        The George Washington University
  --         Department of Electrical Engineering and Computer Science
  --                             Small Ada Schedulers
  --                            Amr El-Kadi  Fall 1990

  procedure  Scheduler_0 (NexTask : in out Integer;  PS : in out  Processor_State) is
     maxpriority, p : Float;
         nready : Integer;
    allcomplete : Boolean := True;  --  check if all tasks are completed
          dummy : Boolean;
  begin
    for t in  0 .. TCount loop
      if ND.TCB (t).TS /= Completed then
        allcomplete := False;
        exit;
      end if;
    end loop;
    if  allcomplete then
      PS := FIN;  --  Stop the SmallAda processor
    else
      nready := 0;
      maxpriority := -1.0;
      for t in 0 .. TCount loop
        --  Of ready tasks, find one w/ max priority
        if ND.TCB (t).TS = Ready then
          p := Calc_Priority (t);
          if  p > maxpriority then
            maxpriority := p;
            NexTask := t;
          end if;
          nready := nready + 1;
        end if;
      end loop;
      if nready = 0 then
        --  No ready tasks ; any delayed ? ; if yes then WAIT
        if Any_Task_Delayed (CD, ND) then
          --  Update SYSCLOCK and do loop again
          Wake_Tasks (CD, ND, dummy);  --  wakes tasks
          PS := WAIT;  --  there are delayed tasks, put processor
                       --  in a wait state
        else  --  no ready tasks, no delayed tasks, DEADLOCK
          PS := DEADLOCK;
        end if;
      else  --  There was at least one ready task ; continue to process
        PS := Running;
        --         if (tcb [0].ts = completed) then
        --         begin
        --           PS := FIN ;
        --           for t := 1 to tcount do
        --           begin
        --             if ((tcb[t].ts = ready) and (not (tcb [t].term))) then
        --             begin
        --               PS := DEADLOCK ;   {Check for deadlock} {Oguz Tumer}
        --             end ;
        --           end ;
        --         end ;
      end if;
    end if;
  end Scheduler_0;

  --  This Scheduler implements the Run Until Blocked strategy
  --              Amr El-Kadi    Fall 1990

  procedure  Scheduler_1 (NextTask : in out Integer;  PS : in out Processor_State) is
    nready             : Integer;
    allcomplete, dummy : Boolean;
  begin
    if ND.TCB (NextTask).TS /= Ready then
      --  If the task was running, it becomes ready now
      if ND.TCB (NextTask).TS = Running then ND.TCB (NextTask).TS := Ready; end if;
      if Any_Task_Delayed (CD, ND) then  --  Check if any is delayed
        Wake_Tasks (CD, ND, dummy);  --  wakes tasks
        PS := WAIT; --  Processor's state is waiting
      end if;
      nready := 0;
      allcomplete := True;
      for t in 0 .. TCount loop  --  Check if all have completed
        if ND.TCB (t).TS /= Completed then
          allcomplete := False;
          if ND.TCB (t).TS = Ready then nready := nready + 1; end if;  --  Count ready tasks
        end if;
      end loop;
      if allcomplete then
        PS := FIN;  --  Stop the Processor
      else
        if nready = 0 and PS /= WAIT then
          PS := DEADLOCK;  --  No ready or delayed tasks
        else
          if  nready /= 0 then  --  There is at least one ready task
            PS := Running;
            --  Search Starting from the current for a ready task
            NextTask := NextTask + 1;
            if NextTask > TCount then NextTask := 0; end if;
            while ND.TCB (NextTask).TS /= Ready loop
              NextTask := NextTask + 1;
              if NextTask > TCount then NextTask := 0; end if;
            end loop;
          end if;
        end if;
      end if;
    else
      PS := Running;
    end if;
  end Scheduler_1;

  --  This Scheduler implements the Round Robin strategy  (using time slices)
  --                      Amr El-Kadi    Fall 1990

  procedure  Scheduler_2 (NextTask : in out Integer; PS : in out Processor_State) is
    nready             : Integer;
    allcomplete, dummy : Boolean;
  begin
    ND.TCB (NextTask).QUANTUM := ND.TCB (NextTask).QUANTUM - 1.0;  --  decrement time given
    if ND.TCB (NextTask).QUANTUM < 0.0 or ND.TCB (NextTask).TS /= Ready then  --  Time slice was exhausted
      --  If the task was running, it becomes ready now
      if ND.TCB (NextTask).TS = Running then ND.TCB (NextTask).TS := Ready; end if;
      if Any_Task_Delayed (CD, ND) then  --  Check if any is delayed
        Wake_Tasks (CD, ND, dummy);  --  wakes tasks
        PS := WAIT; --  Processor's state is waiting
      end if;
      nready := 0;
      allcomplete := True;
      for t in 0 .. TCount loop  --  Check if all have completed
        if ND.TCB (t).TS /= Completed then
          allcomplete := False;
          if ND.TCB (t).TS = Ready then nready := nready + 1; end if;  --  Count ready tasks
        end if;
      end loop;
      if  allcomplete then
        PS := FIN;  --  Stop the Processor
      else
        if nready = 0  and PS /= WAIT then
          PS := DEADLOCK;  --  No ready or delayed tasks
        else
          if nready /= 0 then
            --  There is at least one ready task
            PS := Running;
            --  Search Starting from the current for a ready task
            NextTask := NextTask + 1;
            if NextTask > TCount then NextTask := 0; end if;
            while ND.TCB (NextTask).TS /= Ready loop
              NextTask := NextTask + 1;
              if NextTask > TCount then NextTask := 0; end if;
            end loop;
            ND.TCB (NextTask).QUANTUM := TSlice;  --  give it a time slice
          end if;
        end if;
      end if;
    else
      PS := Running;
    end if;  --  Time slice was not exhausted
  end Scheduler_2;

  --  This procedure searches the list of READY tasks and returns the
  --  index of the Ready Task with the highest priority
  --                      Amr El-Kadi

  procedure highest (t : in out Integer) is
  begin
    t := 0;
    for i in 0 .. TCount loop
      if ND.TCB (i).TS = Ready and ND.TCB (i).Pcontrol.UPRI > ND.TCB (t).Pcontrol.UPRI then
        t := i;
      end if;
    end loop;
  end highest;

  --  This Scheduler implements the Static Priority strategy  (using lexical order)
  --          This does not use time slicing, but it is preemptive
  --                      Amr El-Kadi    Fall 1990

  procedure  Scheduler_3 (NextTask : in out Integer;  PS : in out Processor_State) is
                     nready : Integer;
                allcomplete : Boolean;
                      dummy : Boolean;
  begin
    --  If the task was running, it becomes ready now
    if ND.TCB (NextTask).TS = Running then ND.TCB (NextTask).TS := Ready; end if;
    if Any_Task_Delayed (CD, ND) then  --  Check if any is delayed
      Wake_Tasks (CD, ND, dummy);  --  wakes tasks
      PS := WAIT;  --  Processor's state is waiting
    end if;
    nready := 0;
    allcomplete := True;
    for t in 0 .. TCount loop  --  Check if all have completed
      if ND.TCB (t).TS /= Completed then
        allcomplete := False;
        if ND.TCB (t).TS = Ready then nready := nready + 1; end if; --  Count ready tasks
      end if;
    end loop;
    if  allcomplete then
      PS := FIN;  --  Stop the Processor
    else
      if nready = 0 and PS /= WAIT then
        PS := DEADLOCK;  --  No ready or delayed tasks
      else
        if nready /= 0 then
          --  There is at least one ready task
          PS := Running;
          --  Search for the highest priority task
          highest (NextTask);
        end if;
      end if;
    end if;
  end Scheduler_3;

  --  This Scheduler implements the Static Priority strategy  (using lexical order)
  --            This does use time slicing, and it is not preemptive
  --                      Amr El-Kadi    Fall 1990

  procedure  Scheduler_4 (NextTask : in out  Integer;  PS : in out  Processor_State) is
    nready             : Integer;
    allcomplete, dummy : Boolean;
  begin
    ND.TCB (NextTask).QUANTUM := ND.TCB (NextTask).QUANTUM - 1.0;  --  decrement time given
    if ND.TCB (NextTask).QUANTUM < 0.0 or ND.TCB (NextTask).TS /= Ready then  --  Time slice was exhausted
      --  If the task was running, it becomes ready now
      if ND.TCB (NextTask).TS = Running then ND.TCB (NextTask).TS := Ready; end if;
      if Any_Task_Delayed (CD, ND) then  --  Check if any is delayed
        Wake_Tasks (CD, ND, dummy);  --  wakes tasks
        PS := WAIT; --  Processor's state is waiting
      end if;
      nready := 0;
      allcomplete := True;
      for t in 0 .. TCount loop  --  Check if all have completed
        if ND.TCB (t).TS /= Completed then
          allcomplete := False;
          if ND.TCB (t).TS = Ready then nready := nready + 1; end if; --  Count ready tasks
        end if;
      end loop;
      if  allcomplete then
        PS := FIN;  --  Stop the Processor
      else
        if nready = 0 and PS /= WAIT then
          PS := DEADLOCK;  --  No ready or delayed tasks
        else
          if nready /= 0 then
            --  There is at least one ready task
            PS := Running;
              --  Search for the highest priority task
              highest (NextTask);
              ND.TCB (NextTask).QUANTUM := TSlice; --  give it a time slice
          end if;
        end if;
      end if;
    else
      PS := Running;
    end if; --  Time slice was not exhausted
  end Scheduler_4;

  --  This Scheduler implements the Static Priority strategy  (using lexical order)
  --            This does use time slicing, and it is preemptive
  --                      Amr El-Kadi    Fall 1990

  procedure  Scheduler_5 (NextTask : in out Integer;  PS : in out  Processor_State) is
    tt, nready         : Integer;
    allcomplete, dummy : Boolean;
  begin
    ND.TCB (NextTask).QUANTUM := ND.TCB (NextTask).QUANTUM - 1.0;  --  decrement time given
    --  If the task was running, it becomes ready now
    if ND.TCB (NextTask).TS = Running then ND.TCB (NextTask).TS := Ready; end if;
    if Any_Task_Delayed (CD, ND) then  --  Check if any is delayed
      Wake_Tasks (CD, ND, dummy);  --  wakes tasks
      PS := WAIT; --  Processor's state is waiting
    end if;

    nready := 0;
    allcomplete := True;
    for t in 0 .. TCount loop  --  Check if all have completed
      if ND.TCB (t).TS /= Completed then
        allcomplete := False;
        if ND.TCB (t).TS = Ready then nready := nready + 1; end if;  --  Count ready tasks
      end if;
    end loop;
    if  allcomplete then
      PS := FIN;  --  Stop the Processor
    else
      if nready = 0 and PS /= WAIT then
        PS := DEADLOCK;  --  No ready or delayed tasks
      else
        if nready /= 0 then
           --  There is at least one ready task
          PS := Running;
          if ND.TCB (NextTask).QUANTUM < 0.0 or ND.TCB (NextTask).TS /= Ready  then  --  Time slice was exhausted
            --  Search for the highest priority task
            highest (NextTask);
            ND.TCB (NextTask).QUANTUM := TSlice; --  give it a time slice
          else
            tt := NextTask;
            --  Search for the highest priority task
            highest (tt);
            if tt /= NextTask then --  one with higher priority was found
              ND.TCB (NextTask).QUANTUM := 0.0;  --  reset the time slice for the old one
              NextTask := tt;
              ND.TCB (NextTask).QUANTUM := TSlice; --  give it a time slice
            end if;
          end if;
        end if;
      end if;
    end if;
  end Scheduler_5;

  --  This Scheduler implements the Static Priority strategy  (using lexical order)
  --            This does not use time slicing, and it is not preemptive
  --                      Amr El-Kadi    Fall 1990

  procedure  Scheduler_6 (NextTask : in out Integer; PS : in out Processor_State) is
    nready             : Integer;
    allcomplete, dummy : Boolean;
  begin
    ND.TCB (NextTask).QUANTUM := ND.TCB (NextTask).QUANTUM - 1.0; --  decrement time given
    if ND.TCB (NextTask).TS /= Ready then
      --  If the task was running, it becomes ready now
      if ND.TCB (NextTask).TS = Running then ND.TCB (NextTask).TS := Ready; end if;
      if Any_Task_Delayed (CD, ND) then  --  Check if any is delayed
        Wake_Tasks (CD, ND, dummy);  --  wakes tasks
        PS := WAIT; --  Processor's state is waiting
      end if;
      nready := 0;
      allcomplete := True;
      for t in 0 .. TCount loop  --  Check if all have completed
        if ND.TCB (t).TS /= Completed then
          allcomplete := False;
          if ND.TCB (t).TS = Ready then nready := nready + 1; end if; --  Count ready tasks
        end if;
      end loop;
      if  allcomplete then
        PS := FIN;  --  Stop the Processor
      else
        if nready = 0 and PS /= WAIT then
          PS := DEADLOCK;  --  No ready or delayed tasks
        else
          if nready /= 0 then
            --  There is at least one ready task
            PS := Running;
            --  Search for the highest priority task
            highest (NextTask);
          end if;
        end if;
      end if;
    else
      PS := Running;  --  Task did not block yet
    end if;
  end Scheduler_6;

begin
  case ND.Scheduler is
    when 0 => Scheduler_0 (ND.CurTask, ND.PS);
    when 1 => Scheduler_1 (ND.CurTask, ND.PS);
    when 2 => Scheduler_2 (ND.CurTask, ND.PS);
    when 3 => Scheduler_3 (ND.CurTask, ND.PS);
    when 4 => Scheduler_4 (ND.CurTask, ND.PS);
    when 5 => Scheduler_5 (ND.CurTask, ND.PS);
    when 6 => Scheduler_6 (ND.CurTask, ND.PS);
    when Single_Task => null;
  end case;
end Scheduler;
