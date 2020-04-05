with Ada.Calendar;                      use Ada.Calendar;

with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;

with Ada.Unchecked_Deallocation;

package body HAC.PCode.Interpreter is

  package REF is new Ada.Numerics.Generic_Elementary_Functions (HAC.Data.HAC_Float);
  use REF, RIO;
  use type HAC.Data.HAC_Float;

  package InterDef is -- sub-package, was a separate Turbo Pascal unit

    --  DaysPast:array ( 1 .. 12 ) of  integer := --  Does not account for
    --leap year

    --  (    0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 335  );
    --  Jan  Feb  Mar  Apr  May  Jun  Jul  Aug  Sep  Oct  Nov  Dec

    NilTask : Integer := -1;

    subtype TRange is Integer range 0 .. HAC.Data.TaskMax; --  task index

    type ProcessorState is (
     CASCHK,
    -- ConstErr,
     DEADLOCK,
     DIVCHK,
     FIN,
     INXCHK,
    -- NumErr  ,
     ProgErr,
     REDCHK,
     RUN,
     STKCHK,
    -- StoErr  , TaskErr ,
     WAIT);
    type TaskState is (
     Completed,
     Delayed,
     Ready,
     Running,
     Critical,
     WaitRendzv,
     WaitSem,
     TimedRendz,
     TimedWait,
     Terminated);

    type PriCB is record  --  Priority Control Block
      UPRI    : Integer;  --  User specified priority
      INHERIT : Boolean;  --  Priority inheritance enabled
    end record;

    type GRegister is record
      --  General register - variant record in Pascal
      -- B : Boolean;
      -- C : Character;
      I : Integer; -- Also for former B (Boolean) and C (Character)
      R : HAC.Data.HAC_Float;
    end record;

    subtype Data_Type is GRegister;

    --  Stack
    S : array (1 .. HAC.Data.StMax) of Data_Type;

    subtype file_rng is Integer range 0 .. HAC.Data.FMax;

    type FFiles is array (1 .. HAC.Data.FMax) of Ada.Text_IO.File_Type;
    type FNames is array (1 .. HAC.Data.FMax) of String (1 .. HAC.Data.Alng);

    type FilDescr is record
      CURR  : Integer;
      KOUNT : file_rng;  --  short
      FIL   : FFiles;
      NAM   : FNames;
    end record;

    FAT : FilDescr;                     --  file i/o table

    type Display_type is array (1 .. HAC.Data.LMax) of Integer;

    type TCBrec is record   --  Task Control Block
      --  index of current top of stack
      T : Integer;
      --  index of current base of stack
      B : Integer;
      --  program counter, next pcode
      PC : Integer;
      --  current task state
      TS : TaskState;
      --  task in rendz with or -1
      InRendzv : Integer;
      --  end of delay period
      WAKETIME : Time;
      --  task priority parameter rec.
      Pcontrol : PriCB;
      --  millisecond time slice
      QUANTUM : Duration;
      --  time last run end (fairness)
      LASTRUN : Time;
      --  binding
      DISPLAY : Display_type;
      --  stack overflow if exceeded
      STACKSIZE : Integer;
      --  id of object suspended on
      SUSPEND : Integer;
      --  general use registers
      R1, R2, R3 : GRegister;
    end record;

    type Enode;
    type Eptr is access Enode; --  task entry rendzv pointer

    type Enode is record        --  task entry structure
      Task_Index : TRange; --  index of task enqueued for rendzv
      Next : Eptr; --  next entry in list
    end record;

    procedure Dispose is new Ada.Unchecked_Deallocation (Enode, Eptr);

    type EHeader is record
      Task_Index : TRange; --  index of task that contains entry
      First : Eptr; --  ptr to first node in rendzv queue
      Last : Eptr; --  ptr to last  node in rendzv queue
    end record;

    IR : HAC.Data.Order; --  instruction register

    PS       : ProcessorState:= RUN; --  processor status register
    TCB      : array (TRange) of TCBrec; --  Task control blocks
    EList    : array (1 .. HAC.Data.EntryMax) of EHeader; --  Entry queue array
    SWITCH   : Boolean:= False; --  invoke scheduler on next cycle flag
    SYSCLOCK : Time:= Clock;    --  (ms after 00:00:00 Jan 1, current year)
    TIMER    : Time:= SYSCLOCK; --  set to end of current task's time slice
    TActive  : TRange;  --  no. of active tasks

    CurTask : Integer; --  index of currently executing task

    H1, H2 : Integer;
    H3, H4 : Integer;
    H5     : Integer;
    F1     : HAC.Data.HAC_Float;
    BLKCNT : Integer;

    NCALLS : Integer;   --  AVL  TERMINATE
    NCOMPL : Integer;   --  AVL  TERMINATE

    function GetClock return Time renames Clock;

    SNAP: Boolean; --  SNAP-shot flag To Display sched. status

  end InterDef;

  --  Post Mortem Dump of the task stack causing the exception
  --
  procedure Post_Mortem_Dump is
    use Ada.Text_IO, Ada.Integer_Text_IO, Boolean_Text_IO;
    use InterDef;
  begin
      New_Line;
      Put_Line("Stack Variables of Task " & HAC.Data.IdTab (HAC.Data.TaskDefTab (InterDef.CurTask)).Name);
      InterDef.H1 := InterDef.TCB (InterDef.CurTask).B;   --  current botton
                                                          --of stack
      InterDef.BLKCNT := 10;
      loop
        New_Line;
        InterDef.BLKCNT := InterDef.BLKCNT - 1;
        if InterDef.BLKCNT = 0 then
          InterDef.H1 := 0;
        end if;
        InterDef.H2 := S (InterDef.H1 + 4).I;     --  index into HAC.Data.IdTab for this
                                                  --process
        if InterDef.H1 /= 0 then
          Put (HAC.Data.IdTab (InterDef.H2).Name);
          Put (" CALLED AT");
          Put (S (InterDef.H1 + 1).I, 5);
          New_Line;
        else
          Put_Line ("Task Variables");
        end if;
        InterDef.H2 := HAC.Data.BlockTab (HAC.Data.IdTab (InterDef.H2).Ref).Last;
        while InterDef.H2 /= 0 loop
          -- [P2Ada]: WITH instruction
          declare
            P2Ada_Var_7 : HAC.Data.TabEntry renames HAC.Data.IdTab (InterDef.H2);
            use HAC.Data;
          begin
            if P2Ada_Var_7.Obj = HAC.Data.Variable then
              if P2Ada_Var_7.TYP = Enums or
                 HAC.Data.Standard_Typ (P2Ada_Var_7.TYP)
              then
                if P2Ada_Var_7.Normal then
                  InterDef.H3 := InterDef.H1 + P2Ada_Var_7.Adr;
                else
                  InterDef.H3 := InterDef.S (InterDef.H1 + P2Ada_Var_7.Adr).I;
                end if;
                Put ("  ");
                Put (P2Ada_Var_7.Name);
                Put (" = ");
                case P2Ada_Var_7.TYP is
                  when HAC.Data.Enums | HAC.Data.Ints =>
                    Put (S (H3).I);
                    New_Line;

                  when HAC.Data.Bools =>
                    Put (S (H3).I mod 2 = 1);
                    New_Line;

                  when HAC.Data.Floats =>
                    Put (S (H3).R);
                    New_Line;

                  when HAC.Data.xChars =>
                    Put (S (H3).I);
                    Put_Line (" (ASCII)");

                  when others =>
                    null;  -- [P2Ada]: no otherwise / else in Pascal
                end case;

              end if;
            end if;
            InterDef.H2 := P2Ada_Var_7.Link;

          end; -- [P2Ada]: end of WITH

        end loop;
        InterDef.H1 := InterDef.S (InterDef.H1 + 3).I;
        exit when InterDef.H1 < 0;
      end loop;
  end Post_Mortem_Dump;

  procedure Interpret
    --  Global Data Used (HAC.Data) :
    --       HAC.Data.ObjCode     : Code table, pseudo instructions
    --       S        : Stack
    --       HAC.Data.IdTab         : Table of all identifiers
    --       HAC.Data.ArraysTab     : Array table
    --       HAC.Data.BlockTab      : Block table
    --       FAT      : File I/O table
    --       HAC.Data.FloatPtTab    : Floating point constant table
    --       HAC.Data.StringTab     : String table
    --       HAC.Data.TaskDefTab    : table of Task definitions
    --       HAC.Data.EntryTab      : task Entry table
  is
    Start_Time : constant Time := Clock;
    --  trap label
    Gen : Generator;

    function AnyTaskDelayed return Boolean is
      Result_AnyTaskDelayed : Boolean;

      t           : InterDef.TRange;
      taskdelayed : Boolean;

      use InterDef;

    begin
      taskdelayed := False;
      t           := 0;
      while t <= HAC.Data.TCount and not taskdelayed loop
        taskdelayed := TCB (t).TS = Delayed or
                       TCB (t).TS = TimedRendz or
                       TCB (t).TS = TimedWait;
        t           := t + 1;
      end loop;
      Result_AnyTaskDelayed := taskdelayed;
      return Result_AnyTaskDelayed;
    end AnyTaskDelayed;
    pragma Unreferenced (AnyTaskDelayed);

    function EIndex (Entry_Index : Integer) return Integer is
      i, e : Integer;
    begin
      e := -1;
      i := 1;
      while i <= HAC.Data.ECount and e = -1 loop
        if Entry_Index = HAC.Data.EntryTab (i) then
          e := i;
        end if;
        i := i + 1;
      end loop;
      return e;
    end EIndex;

    procedure Queue(Entry_Index : Integer; CallingTask: InterDef.TRange) is
      ix        : Integer;
      enode_var : InterDef.Eptr;
      use InterDef;
    begin  --  Queue an entry call by CallingTask for entry 'Entry'.
      ix                              := EIndex (Entry_Index);
      enode_var                       := new InterDef.Enode;
      enode_var.Task_Index := CallingTask;
      enode_var.Next                  := null;
      declare
        E_Q_Header : InterDef.EHeader renames InterDef.EList (ix);
      begin
        if E_Q_Header.First = null then
          E_Q_Header.First := enode_var;
        else
          E_Q_Header.Last.Next := enode_var;
        end if;
        E_Q_Header.Last := enode_var;
      end;
    end Queue;

    function FirstCaller (Entry_Index : Integer) return Integer is
      ix, val : Integer;
      use InterDef;
    begin
      ix := EIndex (Entry_Index);
      if InterDef.EList (ix).First = null then
        val := -1;
      else
        val := InterDef.EList (ix).First.Task_Index;
      end if;
      return val;
    end FirstCaller;

    function RemoveFirst
     (Entry_Index : Integer)
      return                   InterDef.TRange
    is
      ix, val : Integer;
      dmy     : InterDef.Eptr;
      use InterDef;
    begin
      ix := EIndex (Entry_Index);
      declare
        E_Q_Header : InterDef.EHeader renames InterDef.EList (ix);
      begin
        val := E_Q_Header.First.Task_Index;
        if E_Q_Header.First = E_Q_Header.Last then
          E_Q_Header.First := null;
          E_Q_Header.Last  := null;
        else
          dmy              := E_Q_Header.First;
          E_Q_Header.First := E_Q_Header.First.Next;
          InterDef.Dispose (dmy);
        end if;
      end;
      return val;
    end RemoveFirst;   --  RemoveFirst

    procedure ShowQ (Entry_Index : Integer)  --  for debugging
    is
      p  : InterDef.Eptr;
      ix : Integer;
      use InterDef, Ada.Text_IO, Ada.Integer_Text_IO;
    begin
      ix := EIndex (Entry_Index);
      p  := InterDef.EList (ix).First;
      Put ("Dumping q for entry ");
      Put (HAC.Data.IdTab (Entry_Index).Name);
      Put (" entry index=");
      Put (ix);
      New_Line;
      if p /= null then
        loop
          Put ("Task ");
          Put (HAC.Data.IdTab (HAC.Data.TaskDefTab (p.Task_Index)).Name);
          New_Line;
          p := p.Next;
          exit when p = null;
        end loop;
      else
        Put ("*** EMPTY ***");
        New_Line;
      end if;
    end ShowQ;
    pragma Unreferenced (ShowQ);

    procedure Purge (Entry_Index : Integer; t : InterDef.TRange) is
      p, q : InterDef.Eptr;     --  has timed out, the entry
      ix : Integer;     --  is purged from the q.
      use InterDef;
    begin
      ix := EIndex (Entry_Index);
      q  := null;
      p  := EList (ix).First;
      while p /= null loop
        if p.Task_Index = t then
          if EList (ix).First = EList (ix).Last then
            EList (ix).First := null;
            EList (ix).Last  := null;
          else
            if p = EList (ix).First then
              EList (ix).First := p.Next;
            else
              if p = EList (ix).Last then
                EList (ix).Last := q;
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

    function TasksToWake return Boolean is
      Result_TasksToWake : Boolean;
      count              : Integer;
      use InterDef;
    begin
      count := 0;
      for t in 0 .. HAC.Data.TCount loop
        if (TCB (t).TS = Delayed or
            TCB (t).TS = TimedRendz or
            TCB (t).TS = TimedWait) and
           SYSCLOCK >= TCB (t).WAKETIME
        then
          if TCB (t).TS = TimedRendz then
            TCB (t).R1.I := 0; --  timeout on rendezvous
            Purge (TCB (t).R2.I, t);  --  remove from callee's q
          end if;
          if TCB (t).TS = TimedWait then
            TCB (t).PC := TCB (t).R1.I; --  t.out on accept
          end if;
          TCB (t).TS := Ready;
          count      := count + 1;
        end if;
      end loop;
      Result_TasksToWake := (count > 0);
      return Result_TasksToWake;
    end TasksToWake;

    --  $I sched.pas
    --  This file contains the different scheduling strategies

    use InterDef;

    procedure Init_main_task is
    begin
      Reset(Gen); --  initialize TPC random number generator
      --  After compiled, just begin exec
      --  Initialize run-time stack
      S (1).I := 0 ;
      S (2).I := 0 ;
      S (3).I := -1 ;
      S (4).I := HAC.Data.TaskDefTab (0) ;
      declare
        Main_TCB : TCBrec renames TCB(0);
      begin
        Main_TCB.PC := HAC.Data.IdTab (HAC.Data.TaskDefTab (0)).Adr ; --  first pcode instruction
        Main_TCB.T := HAC.Data.BlockTab (1).VSize - 1 ; -- was BlockTab (2)
        Main_TCB.B := 0 ;
        Main_TCB.TS := Ready ;
        Main_TCB.InRendzv := NilTask ;
        Main_TCB.DISPLAY (1) := 0 ;
        Main_TCB.STACKSIZE := HAC.Data.StMax - (HAC.Data.TCount * HAC.Data.STKINCR) ;
        Main_TCB.SUSPEND := 0 ;
        Main_TCB.QUANTUM := Duration(HAC.Data.TSlice);
        Main_TCB.Pcontrol.UPRI := 0 ;
        Main_TCB.Pcontrol.INHERIT := False ;
        Main_TCB.LASTRUN := Start_Time ;
      end;
    end Init_main_task;

    procedure Init_other_tasks is
    begin
      for CurTask  in  1 .. HAC.Data.TCount loop
        declare
          Curr_TCB : TCBrec renames TCB(CurTask);
        begin
          H1 := HAC.Data.TaskDefTab (CurTask) ;
          Curr_TCB.PC := HAC.Data.IdTab (H1).Adr ;
          Curr_TCB.B := TCB (CurTask - 1).STACKSIZE + 1 ;
          Curr_TCB.T := Curr_TCB.B + HAC.Data.BlockTab (HAC.Data.IdTab (H1).Ref).VSize - 1 ;
          S (Curr_TCB.B + 1).I := 0 ;
          S (Curr_TCB.B + 2).I := 0 ;
          S (Curr_TCB.B + 3).I := -1 ;
          S (Curr_TCB.B + 4).I := H1 ;
          Curr_TCB.DISPLAY (1) := 0 ;
          Curr_TCB.DISPLAY (2) := Curr_TCB.B ;
          Curr_TCB.STACKSIZE := Curr_TCB.B + HAC.Data.STKINCR - 1 ;
          Curr_TCB.SUSPEND := 0 ;
          Curr_TCB.TS := Ready ;
          Curr_TCB.InRendzv := NilTask ;
          Curr_TCB.QUANTUM := Duration(HAC.Data.TSlice);
          Curr_TCB.Pcontrol.UPRI := 0 ;
          Curr_TCB.Pcontrol.INHERIT := False ;
          Curr_TCB.LASTRUN := Start_Time ;
        end;
      end loop;
      --  Initially no queued entry calls
      for H1  in  1 .. HAC.Data.ECount loop
        EList (H1).Task_Index := HAC.Data.IdTab (HAC.Data.EntryTab (H1)).Adr ; --  Task index
        EList (H1).First := null ;
        EList (H1).Last  := null ;
      end loop;
      TActive := HAC.Data.TCount ; --  All tasks are active initially
      CurTask := 0 ;  --  IT WAS -1 ?
      SWITCH := True ;
      TIMER := Start_Time; -- was 0.0
      PS := RUN ;
    end Init_other_tasks;

  begin  --  Interpret
    InterDef.SNAP:= False;
    Init_main_task;
    Init_other_tasks;
    loop  --  until Processor state /= RUN
      SYSCLOCK := GetClock;
      if InterDef.SNAP then
        null;  --  ShowTime ;
      end if;
      if InterDef.TCB (InterDef.CurTask).TS = InterDef.Critical then
        if InterDef.SNAP then
          null;  --  SnapShot ;
        end if;
      else
        if InterDef.SWITCH or  --  ------------> Voluntary release of control
           InterDef.SYSCLOCK >= InterDef.TIMER or   --  ---> Time slice
                                                    --exceeded
           TasksToWake
        then --  ------> Awakened task causes switch
          if InterDef.CurTask >= 0 then
            TCB (CurTask).LASTRUN := SYSCLOCK;
            if TCB (CurTask).TS = Running then
              TCB (CurTask).TS := Ready;
              --  SWITCH PROCCESS
            end if;
          end if;
          loop --  Call Main Scheduler
            --  Schedule(Scheduler,CurTask, PS);
            PS := RUN; -- !! Should call the task scheduler instead !!
            InterDef.SYSCLOCK := GetClock;
            if InterDef.SNAP then
              null; --ShowTime ;
            end if;
            if InterDef.SNAP then
              null; -- SnapShot ;
            end if;
            exit when PS /= WAIT;
          end loop;
          if PS = DEADLOCK or PS = FIN then
            goto LABEL_123777;
          end if;
          TIMER:= InterDef.SYSCLOCK + InterDef.TCB (InterDef.CurTask).QUANTUM;
          InterDef.TCB (InterDef.CurTask).TS := Running;
          SWITCH:= False;
          if InterDef.SNAP then
            null; -- SnapShot ;
          end if;
        end if;
      end if;
      --  FETCH INSTRUCTION

      declare
        Curr_TCB : InterDef.TCBrec renames InterDef.TCB (InterDef.CurTask);
      begin
        InterDef.IR := HAC.Data.ObjCode (Curr_TCB.PC);
        Curr_TCB.PC := Curr_TCB.PC + 1;
      end;

      --  HERE IS THE POINT WHERE THE TASK MONITORING IS CALLED
      -- (removed)

      declare
        Curr_TCB : InterDef.TCBrec renames InterDef.TCB (InterDef.CurTask);
      begin
        case InterDef.IR.F is

          when k_Load_Address =>
            Curr_TCB.T := Curr_TCB.T + 1;
            if Curr_TCB.T > Curr_TCB.STACKSIZE then
              PS := STKCHK;
            else
              S (Curr_TCB.T).I := Curr_TCB.DISPLAY (IR.X) + IR.Y;
            end if;

          when k_Push_Value =>
            Curr_TCB.T := Curr_TCB.T + 1;
            if Curr_TCB.T > Curr_TCB.STACKSIZE then
              PS := STKCHK;
            else
              S (Curr_TCB.T) := S (Curr_TCB.DISPLAY (IR.X) + IR.Y);
            end if;

          when k_Push_Indirect_Value =>
            Curr_TCB.T := Curr_TCB.T + 1;
            if Curr_TCB.T > Curr_TCB.STACKSIZE then
              PS := STKCHK;
            else
              S (Curr_TCB.T) := S (S (Curr_TCB.DISPLAY (IR.X) + IR.Y).I);
            end if;

          when k_Update_Display_Vector =>
            H1 := IR.Y;
            H2 := IR.X;
            H3 := Curr_TCB.B;
            loop
              Curr_TCB.DISPLAY (H1) := H3;
              H1                       := H1 - 1;
              H3                       := S (H3 + 2).I;
              exit when H1 = H2;
            end loop;

          when k_Accept_Rendezvous => -- Hathorn, Cramer
            H1 := IR.Y;                    --  entry pointer
            H2 := FirstCaller (H1);        --  first waiting task
            H3 := HAC.Data.IdTab (H1).LEV;            --  level of accepting entry
            if H2 >= 0 then
              --  start rendzv if call is waiting
              Curr_TCB.DISPLAY (H3 + 1) := TCB (H2).B; --  address callers
              --parms
              Curr_TCB.InRendzv := H2;  --  indicate that task is in Rendzv
              if TCB (H2).TS = TimedRendz then
                TCB (H2).TS := WaitRendzv;
              end if;
            else
              --  or put self to sleep
              Curr_TCB.SUSPEND := H1;
              Curr_TCB.TS      := WaitRendzv;      --  status is waiting for
              --rendezvous
              Curr_TCB.PC      := Curr_TCB.PC - 1;          --  do this
              --step again when awakened

            end if;
            SWITCH := True;

          when k_End_Rendezvous => --  Hathorn
            Curr_TCB.InRendzv := NilTask;  --  indicate rendezvous has ended
            H1 := IR.Y;                   --  entry pointer
            H2 := RemoveFirst (H1);       --  waiting task pointer
            if H2 >= 0 then
              --  wake up waiting task
              TCB (H2).SUSPEND := 0;
              TCB (H2).TS      := Ready;
              SWITCH           := True;
            end if;

          when k_Wait_Semaphore =>
            H1            := S (Curr_TCB.T).I;
            Curr_TCB.T := Curr_TCB.T - 1;
            if S (H1).I > 0 then
              S (H1).I       := S (H1).I - 1;
              Curr_TCB.TS := Critical;   --  In a critical section, task gets
              --  exclusive access to the virtual
            else
              --  processor until section ends.
              Curr_TCB.SUSPEND := H1;
              Curr_TCB.TS      := WaitSem;
              SWITCH              := True;
            end if;

          when k_Signal_Semaphore =>
            H1            := S (Curr_TCB.T).I;
            Curr_TCB.T := Curr_TCB.T - 1;
            H2            := HAC.Data.TCount + 1;
            H3            := Integer (Random (Gen) * Float (H2));
            while H2 >= 0 and TCB (H3).TS /= WaitSem and TCB (H3).SUSPEND /= H1
            loop
              H3 := (H3 + 1) mod (HAC.Data.TaskMax + 1);
              H2 := H2 - 1;
            end loop;
            if H2 < 0 or S (H1).I < 0 then
              S (H1).I := S (H1).I + 1;
            else
              TCB (H3).SUSPEND := 0;
              TCB (H3).TS      := Ready;
            end if;
            Curr_TCB.TS := Ready; --  end critical section
            SWITCH := True;

          when k_Standard_Functions =>

            case IR.Y is
              when 0 =>
                S (Curr_TCB.T).I := abs (S (Curr_TCB.T).I);
              when 1 =>
                S (Curr_TCB.T).R := abs (S (Curr_TCB.T).R);
              when 2 =>
                S (Curr_TCB.T).I := ((S (Curr_TCB.T).I) ** 2);
              when 3 =>
                S (Curr_TCB.T).R := ((S (Curr_TCB.T).R) ** 2);
              when 4 => -- Odd
                null;
              when 5 => -- Chr = Character'Val
                if (S (Curr_TCB.T).I < HAC.Data.OrdMinChar) or
                  (S (Curr_TCB.T).I > HAC.Data.OrdMaxChar)
                then
                  PS := INXCHK;
                end if;
              when 6 => -- Ord = Character'Pos
                null;
              when 7 =>
                S (Curr_TCB.T).I := S (Curr_TCB.T).I + 1;
              when 8 =>
                S (Curr_TCB.T).I := S (Curr_TCB.T).I - 1;
              when SF_Round_Float_to_Int =>
                S (Curr_TCB.T).I := Integer (S (Curr_TCB.T).R);
              when 10 =>  --  Trunc
                S (Curr_TCB.T).I := Integer (HAC.Data.HAC_Float'Floor (S (Curr_TCB.T).R));
              when 11 =>
                S (Curr_TCB.T).R := Sin (S (Curr_TCB.T).R);
              when 12 =>
                S (Curr_TCB.T).R := Cos (S (Curr_TCB.T).R);
              when 13 =>
                S (Curr_TCB.T).R := Exp (S (Curr_TCB.T).R);
              when 14 =>
                S (Curr_TCB.T).R := Log (S (Curr_TCB.T).R);
              when 15 =>
                S (Curr_TCB.T).R := Sqrt (S (Curr_TCB.T).R);
              when 16 =>
                S (Curr_TCB.T).R := Arctan (S (Curr_TCB.T).R);
              when 17 =>
                Curr_TCB.T := Curr_TCB.T + 1;
                if Curr_TCB.T > Curr_TCB.STACKSIZE then
                  PS := STKCHK;
                else
                  if IR.X = 0 then
                    S (Curr_TCB.T).I := Boolean'Pos(End_Of_File_Console);
                  else
                    S (Curr_TCB.T).I := Boolean'Pos(Ada.Text_IO.End_Of_File (FAT.FIL (IR.X)));
                  end if;
                end if;
              when 18 =>
                Curr_TCB.T := Curr_TCB.T + 1;
                if Curr_TCB.T > Curr_TCB.STACKSIZE then
                  PS := STKCHK;
                else
                  if IR.X = 0 then
                    S (Curr_TCB.T).I := Boolean'Pos(End_Of_Line_Console);
                  else
                    S (Curr_TCB.T).I := Boolean'Pos(Ada.Text_IO.End_Of_Line (FAT.FIL (IR.X)));
                  end if;
                end if;
              when 19 =>
                S (Curr_TCB.T).I :=
                  Integer (HAC.Data.HAC_Float (Random (Gen)) *
                           HAC.Data.HAC_Float ((S (Curr_TCB.T).I + 1)));
              when 100 =>
                --  CLOCK function, NILADIC functions have IR.Y => 100
                --  Return time of units of seconds.
                Curr_TCB.T := Curr_TCB.T + 1;
                if Curr_TCB.T > Curr_TCB.STACKSIZE then
                  PS := STKCHK;
                else
                  S (Curr_TCB.T).R := HAC.Data.HAC_Float (GetClock - Start_Time);
                end if;
              when others =>
                null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;

        when k_Offset =>
          S (Curr_TCB.T).I := S (Curr_TCB.T).I + IR.Y;

        when k_Jump =>
          Curr_TCB.PC := IR.Y;

        when k_Conditional_Jump =>
          if S (Curr_TCB.T).I mod 2 = 0 then
            Curr_TCB.PC := IR.Y;
          end if;
          Curr_TCB.T := Curr_TCB.T - 1;

        when 12 => --  SWTC - switch
          H1            := S (Curr_TCB.T).I;
          Curr_TCB.T := Curr_TCB.T - 1;
          H2            := IR.Y;
          H3            := 0;
          loop
            if HAC.Data.ObjCode (H2).F /= 13 then
              H3 := 1;
              PS := CASCHK;
            else
              if HAC.Data.ObjCode (H2).Y = H1 or HAC.Data.ObjCode (H2).X < 0 then
                H3             := 1;
                Curr_TCB.PC := HAC.Data.ObjCode (H2 + 1).Y;
              else
                H2 := H2 + 2;
              end if;
            end if;
            exit when H3 /= 0;
          end loop;

        when 14 => --  FOR1
          H1 := S (Curr_TCB.T - 1).I;
          if H1 <= S (Curr_TCB.T).I then
            S (S (Curr_TCB.T - 2).I).I := H1;
          else
            Curr_TCB.T  := Curr_TCB.T - 3;
            Curr_TCB.PC := IR.Y;
          end if;

        when 15 => --  FOR2
          H2 := S (Curr_TCB.T - 2).I;
          H1 := S (H2).I + 1;
          if H1 <= S (Curr_TCB.T).I then
            S (H2).I       := H1;
            Curr_TCB.PC := IR.Y;
          else
            Curr_TCB.T := Curr_TCB.T - 3;
          end if;

        when 16 => --  for1rev
          H1 := S (Curr_TCB.T).I;
          if H1 >= S (Curr_TCB.T - 1).I then
            S (S (Curr_TCB.T - 2).I).I := H1;
          else
            Curr_TCB.PC := IR.Y;
            Curr_TCB.T  := Curr_TCB.T - 3;
          end if;

        when 17 => --  for2rev
          H2 := S (Curr_TCB.T - 2).I;
          H1 := S (H2).I - 1;
          if H1 >= S (Curr_TCB.T - 1).I then
            S (H2).I       := H1;
            Curr_TCB.PC := IR.Y;
          else
            Curr_TCB.T := Curr_TCB.T - 3;
          end if;

        when 18 => --  mark stack
          H1 := HAC.Data.BlockTab (HAC.Data.IdTab (IR.Y).Ref).VSize;
          if Curr_TCB.T + H1 > Curr_TCB.STACKSIZE then
            PS := STKCHK;
          else
            Curr_TCB.T := Curr_TCB.T + 5; --  make room for fixed area
            S (Curr_TCB.T - 1).I := H1 - 1; --  vsize-1
            S (Curr_TCB.T).I := IR.Y;    --  HAC.Data.IdTab index of called
                                            --procedure/entry
          end if;

        when 19 =>
          --  procedure and task entry CALL
          --  Cramer
          if IR.X = HAC.Data.CallTMDE then
            --  Timed entry call
            F1 := S (Curr_TCB.T).R; --  Pop delay time
            Curr_TCB.T := Curr_TCB.T - 1;
          end if;
          H1 := Curr_TCB.T - IR.Y;     --  base of activation record
          H2 := S (H1 + 4).I; --  HAC.Data.IdTab index of called procedure/entry
          H3                           := HAC.Data.IdTab (H2).LEV;
          Curr_TCB.DISPLAY (H3 + 1) := H1;
          S (H1 + 1).I                 := Curr_TCB.PC; --  return address

          H4 := S (H1 + 3).I + H1; --  new top of stack
          S (H1 + 2).I := Curr_TCB.DISPLAY (H3); --  static link
          S (H1 + 3).I := Curr_TCB.B; --  dynamic link

          for H3 in Curr_TCB.T + 1 .. H4 loop
            S (H3).I := 0; --  initialize local vars
          end loop;
          Curr_TCB.B := H1;
          Curr_TCB.T := H4;
          case IR.X is
            when  --  Call type
             HAC.Data.CallSTDP =>
              --  Standard procedure call

              Curr_TCB.PC := HAC.Data.IdTab (H2).Adr;

            when HAC.Data.CallSTDE =>
              --  Unconditional entry call
              Queue (H2, CurTask);          --  put self on entry queue
              Curr_TCB.TS := WaitRendzv;
              H5             := HAC.Data.IdTab (H2).Adr;               --  Task being
                                                            --entered

              if ((TCB (H5).TS = WaitRendzv) and (TCB (H5).SUSPEND = H2)) or
                 (TCB (H5).TS = TimedWait)
              then
                --  wake accepting task if necessayr

                TCB (H5).TS      := Ready;
                TCB (H5).SUSPEND := 0;
              end if;
              SWITCH := True;                 --  give up control

            when HAC.Data.CallTMDE =>
              --  Timed entry call
              Queue (H2, CurTask);    --  put self on entry queue
              H5 := HAC.Data.IdTab (H2).Adr;    --  Task being entered

              if ((TCB (H5).TS = WaitRendzv) and (TCB (H5).SUSPEND = H2)) or
                 (TCB (H5).TS = TimedWait)
              then
                --  wake accepting task if necessary
                Curr_TCB.TS := WaitRendzv;     --  suspend self
                TCB (H5).TS := Ready;  --  wake accepting task
                TCB (H5).SUSPEND := 0;
              else
                Curr_TCB.TS := TimedRendz;     --  Timed Wait For Rendezvous
                Curr_TCB.R1.I := 1;            --  Init R1 to specify NO
                                                  --timeout
                Curr_TCB.R2.I := H2;           --  Save address of queue
                                                  --for purge
                SYSCLOCK := GetClock; --  update System Clock
                Curr_TCB.WAKETIME := SYSCLOCK + Duration (F1 * 1000.0);
                --  internal time units is milliseconds so X 1000.0
              end if;
              SWITCH := True;       --  give up control

            when HAC.Data.CallCNDE =>
              --  Conditional Entry Call
              H5 := HAC.Data.IdTab (H2).Adr;              --  Task being entered
              if ((TCB (H5).TS = WaitRendzv) and (TCB (H5).SUSPEND = H2)) or
                 (TCB (H5).TS = TimedWait)
              then
                Queue (H2, CurTask);    --  put self on entry queue
                Curr_TCB.R1.I := 1;       --  Indicate entry successful
                Curr_TCB.TS := WaitRendzv;
                TCB (H5).TS    := Ready;  --  wake accepting task if required
                TCB (H5).SUSPEND := 0;
                SWITCH           := True;       --  give up control
              else
                --  can't wait, forget about entry call
                Curr_TCB.R1.I := 0;   --  Indicate entry failed in R1 1
                --  failure will be acknowledged by next instruction, 32
              end if;
            when others =>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;

        when 20 => --  INDEX1
          H1 := IR.Y;     --  H1 points to HAC.Data.ArraysTab
          H2 := HAC.Data.ArraysTab (H1).Low;
          H3 := S (Curr_TCB.T).I;
          if H3 < H2 then
            PS := INXCHK;
          else
            if H3 > HAC.Data.ArraysTab (H1).High then
              PS := INXCHK;
            else
              Curr_TCB.T       := Curr_TCB.T - 1;
              S (Curr_TCB.T).I := S (Curr_TCB.T).I + (H3 - H2);
            end if;
          end if;

        when 21 => --  INDEX
          H1 := IR.Y;      --  H1 POINTS TO HAC.Data.ArraysTab
          H2 := HAC.Data.ArraysTab (H1).Low;
          H3 := S (Curr_TCB.T).I;
          if H3 < H2 then
            PS := INXCHK;
          else
            if H3 > HAC.Data.ArraysTab (H1).High then
              PS := INXCHK;
            else
              Curr_TCB.T       := Curr_TCB.T - 1;
              S (Curr_TCB.T).I := S (Curr_TCB.T).I +
                                     (H3 - H2) * HAC.Data.ArraysTab (H1).ELSize;
            end if;
          end if;

        when k_Load_Block =>
          H1            := S (Curr_TCB.T).I;
          Curr_TCB.T := Curr_TCB.T - 1;
          H2            := IR.Y + Curr_TCB.T;
          if H2 > Curr_TCB.STACKSIZE then
            PS := STKCHK;
          else
            while Curr_TCB.T < H2 loop
              Curr_TCB.T     := Curr_TCB.T + 1;
              S (Curr_TCB.T) := S (H1);
              H1                := H1 + 1;
            end loop;
          end if;

        when k_Copy_Block =>
          H1 := S (Curr_TCB.T - 1).I;
          H2 := S (Curr_TCB.T).I;
          H3 := H1 + IR.Y;
          while H1 < H3 loop
            S (H1) := S (H2);
            H1     := H1 + 1;
            H2     := H2 + 1;
          end loop;
          Curr_TCB.T := Curr_TCB.T - 2;

        when k_Literal => --  Literal (Integer or Character)
          Curr_TCB.T := Curr_TCB.T + 1;
          if Curr_TCB.T > Curr_TCB.STACKSIZE then
            PS := STKCHK;
          else
            S (Curr_TCB.T).I := IR.Y;
          end if;

        when k_Load_Float =>
          Curr_TCB.T := Curr_TCB.T + 1;
          if Curr_TCB.T > Curr_TCB.STACKSIZE then
            PS := STKCHK;
          else
            S (Curr_TCB.T).R := HAC.Data.FloatPtTab (IR.Y);
          end if;

        when k_Integer_to_Float =>
          H1       := Curr_TCB.T - IR.Y;
          S (H1).R := HAC.Data.HAC_Float (S (H1).I);

        when k_Read =>
          --  READ
          if FAT.CURR = 0 then
            if End_Of_File_Console then
              PS := REDCHK;
            else
              case IR.Y is
                when 1 =>
                  Get_Console (S (S (Curr_TCB.T).I).I);
                when 2 =>
                  Get_Console (S (S (Curr_TCB.T).I).R);
                when 3 =>
                  Get_Console (HAC.Data.CH);
                  S (S (Curr_TCB.T).I).I := Character'Pos (HAC.Data.CH);
                when 4 =>
                  declare
                    C: Character;
                  begin
                    Get_Console (C);
                    S (S (Curr_TCB.T).I).I:= Character'Pos(C);
                  end;
                when others =>
                  null;  -- [P2Ada]: no otherwise / else in Pascal
              end case;
            end if;
            Curr_TCB.T := Curr_TCB.T - 1;
          else
            if Ada.Text_IO.End_Of_File (FAT.FIL (FAT.CURR)) then
              PS := REDCHK;
            else
              case IR.Y is
                when 1 =>
                  Ada.Integer_Text_IO.Get (FAT.FIL (FAT.CURR), S (S (Curr_TCB.T).I).I);
                when 2 =>
                  RIO.Get (FAT.FIL (FAT.CURR), S (S (Curr_TCB.T).I).R);
                when 3 =>
                  Ada.Text_IO.Get (FAT.FIL (FAT.CURR), HAC.Data.CH);
                  S (S (Curr_TCB.T).I).I := Character'Pos (HAC.Data.CH);
                when 4 =>
                  declare
                    C: Character;
                  begin
                    Ada.Text_IO.Get(FAT.FIL (FAT.CURR), C);
                    S (S (Curr_TCB.T).I).I:= Character'Pos(C);
                  end;
                when others =>
                  null;  -- [P2Ada]: no otherwise / else in Pascal
              end case;
            end if;
          end if;
          SWITCH := True;  --  give up control when doing I/O

        when k_Write_String =>
          H1 := S (Curr_TCB.T).I;   --  length of string
          H2 := IR.Y;     --  pointer to 1st char in string
          Curr_TCB.T := Curr_TCB.T - 1;
          while H1 > 0 loop
            if FAT.CURR = 0 then
              Put_Console (HAC.Data.StringTab (H2));
            else
              Ada.Text_IO.Put (FAT.FIL (FAT.CURR), HAC.Data.StringTab (H2));
            end if;
            H1 := H1 - 1;        --  decrement length
            H2 := H2 + 1;
            --  increment char pointer
          end loop;
          SWITCH := True;        --  give up control when doing I/O

        when 29 =>
          --  write1
          if FAT.CURR = 0 then
            case IR.Y is
              when 1 =>   --  Burd
                Put_Console (S (Curr_TCB.T).I);
              when 2 =>
                Put_Console (S (Curr_TCB.T).R);
              when 3 =>
                Put_Console (Boolean'Image(Boolean'Val(S (Curr_TCB.T).I)));
              when 4 =>
                Put_Console (Character'Val(S (Curr_TCB.T).I));
              when others =>
                null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;
          else
            case IR.Y is
              when 1 =>     --  Schoening
                Ada.Integer_Text_IO.Put (FAT.FIL (FAT.CURR), S (Curr_TCB.T).I, 10);
              when 2 =>
                RIO.Put (FAT.FIL (FAT.CURR), S (Curr_TCB.T).R, 22);
              when 3 =>
                Boolean_Text_IO.Put (FAT.FIL (FAT.CURR), Boolean'Val(S (Curr_TCB.T).I), 10);
              when 4 =>
                Ada.Text_IO.Put (FAT.FIL (FAT.CURR), Character'Val(S (Curr_TCB.T).I));
              when others =>
                null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;
          end if;
          Curr_TCB.T := Curr_TCB.T - 1;
          SWITCH        := True;  --  give up control when doing I/O

        when 30 =>
          --  write2
          if FAT.CURR = 0 then
            case IR.Y is
              when 1 => --  Burd
                Put_Console (S (Curr_TCB.T - 1).I, S (Curr_TCB.T).I);
              when 2 =>
                Put_Console (S (Curr_TCB.T - 1).R, S (Curr_TCB.T).I);
              when 3 =>
                Put_Console (Boolean'Val(S (Curr_TCB.T - 1).I), S (Curr_TCB.T).I);
              when 4 =>
                Put_Console (Character'Val(S (Curr_TCB.T - 1).I));
              when others =>
                null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;
          else
            case IR.Y is
              when 1 =>         --  Schoening
                Ada.Integer_Text_IO.Put
                 (FAT.FIL (FAT.CURR),
                  S (Curr_TCB.T - 1).I,
                  S (Curr_TCB.T).I);
              when 2 =>
                RIO.Put
                 (FAT.FIL (FAT.CURR),
                  S (Curr_TCB.T - 1).R,
                  S (Curr_TCB.T).I);
              when 3 =>
                Boolean_Text_IO.Put
                 (FAT.FIL (FAT.CURR),
                  Boolean'Val(S (Curr_TCB.T - 1).I),
                  S (Curr_TCB.T).I);
              when 4 =>
                Ada.Text_IO.Put (FAT.FIL (FAT.CURR), Character'Val(S (Curr_TCB.T - 1).I));
              when others =>
                null;  -- [P2Ada]: no otherwise / else in Pascal
            end case;

          end if;
          Curr_TCB.T := Curr_TCB.T - 2;
          SWITCH        := True;  --  give up control when doing I/O

        when k_Exit_Call =>  --  EXIT entry call or procedure call
          --  Cramer
          Curr_TCB.T := Curr_TCB.B - 1;
          if IR.Y = HAC.Data.CallSTDP then
            Curr_TCB.PC := S (Curr_TCB.B + 1).I;  --  Standard proc call
                                                        --return
          end if;
          if Curr_TCB.PC /= 0 then
            Curr_TCB.B := S (Curr_TCB.B + 3).I;
            if IR.Y = HAC.Data.CallTMDE or IR.Y = HAC.Data.CallCNDE then
              if IR.Y = HAC.Data.CallTMDE and Curr_TCB.R1.I mod 2 = 0 then
                Curr_TCB.T := Curr_TCB.T + 1;         --  A JMPC
                                                            --instruction
                                                            --always follows
              end if;
              if Curr_TCB.T > Curr_TCB.STACKSIZE then --  timed and
                                                            --conditional
                                                            --entry call
                PS := STKCHK;      --  returns (32).  Push entry call
              else
                S (Curr_TCB.T).I := Curr_TCB.R1.I;    --  success
                                                            --indicator for
                                                            --JMPC.
              end if;
            end if;
          else
            TActive        := TActive - 1;
            Curr_TCB.TS := Completed;
            SWITCH         := True;
          end if;

        when k_Exit_Function =>
          Curr_TCB.T  := Curr_TCB.B;
          Curr_TCB.PC := S (Curr_TCB.B + 1).I;
          Curr_TCB.B  := S (Curr_TCB.B + 3).I;
          if IR.Y < 0 then
            PS := ProgErr;
          end if;

        when kCase34 =>
          S (Curr_TCB.T) := S (S (Curr_TCB.T).I);

        when k_NOT_Boolean =>
          S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).I mod 2 = 0);

        when k_Unary_MINUS_Integer =>
          S (Curr_TCB.T).I := -S (Curr_TCB.T).I;

        when k_Unary_MINUS_Float =>
          S (Curr_TCB.T).R := -S (Curr_TCB.T).R;

        when k_Write_Float =>  --  Put Float with 3 parameters
          if FAT.CURR = 0 then
            Put_Console
             (S (Curr_TCB.T - 2).R,
              S (Curr_TCB.T - 1).I,
              S (Curr_TCB.T).I,
              0);
          else
            RIO.Put
             (FAT.FIL (FAT.CURR),
              S (Curr_TCB.T - 2).R,
              S (Curr_TCB.T - 1).I,
              S (Curr_TCB.T).I,
              0);
          end if;
          Curr_TCB.T := Curr_TCB.T - 3;
          SWITCH        := True;  --  give up control when doing I/O

        when k_Store =>
          S (S (Curr_TCB.T - 1).I) := S (Curr_TCB.T);
          Curr_TCB.T               := Curr_TCB.T - 2;

        when k_EQL_Float .. k_DIV_Float | k_XOR_Boolean |
             k_Power_Integer .. k_Power_Float =>
          Curr_TCB.T := Curr_TCB.T - 1;
          case IR.F is
            when k_EQL_Float =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).R = S (Curr_TCB.T + 1).R);

            when k_NEQ_Float =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).R /= S (Curr_TCB.T + 1).R);

            when k_LSS_Float =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).R < S (Curr_TCB.T + 1).R);

            when k_LEQ_Float =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).R <= S (Curr_TCB.T + 1).R);

            when k_GTR_Float =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).R > S (Curr_TCB.T + 1).R);

            when k_GEQ_Float =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).R >= S (Curr_TCB.T + 1).R);

            when k_EQL_Integer =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).I = S (Curr_TCB.T + 1).I);

            when k_NEQ_Integer =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).I /= S (Curr_TCB.T + 1).I);

            when k_LSS_Integer =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).I < S (Curr_TCB.T + 1).I);

            when k_LEQ_Integer =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).I <= S (Curr_TCB.T + 1).I);

            when k_GTR_Integer =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).I > S (Curr_TCB.T + 1).I);

            when k_GEQ_Integer =>
              S (Curr_TCB.T).I := Boolean'Pos(S (Curr_TCB.T).I >= S (Curr_TCB.T + 1).I);

            when k_OR_Boolean =>
              S (Curr_TCB.T).I :=
                Boolean'Pos(S (Curr_TCB.T).I mod 2 = 1 or S (Curr_TCB.T + 1).I mod 2 = 1);

            when k_XOR_Boolean =>
              S (Curr_TCB.T).I :=
                Boolean'Pos(S (Curr_TCB.T).I mod 2 = 1 xor S (Curr_TCB.T + 1).I mod 2 = 1);

            when k_ADD_Integer =>
              S (Curr_TCB.T).I := S (Curr_TCB.T).I + S (Curr_TCB.T + 1).I;

            when k_SUBTRACT_Integer =>
              S (Curr_TCB.T).I := S (Curr_TCB.T).I - S (Curr_TCB.T + 1).I;

            when k_ADD_Float =>
              S (Curr_TCB.T).R := S (Curr_TCB.T).R + S (Curr_TCB.T + 1).R;

            when k_SUBTRACT_Float =>
              S (Curr_TCB.T).R := S (Curr_TCB.T).R - S (Curr_TCB.T + 1).R;

            when k_AND_Boolean =>
              S (Curr_TCB.T).I :=
                Boolean'Pos(S (Curr_TCB.T).I mod 2 =1 and S (Curr_TCB.T + 1).I mod 2 =1);

            when k_MULT_Integer =>
              S (Curr_TCB.T).I := S (Curr_TCB.T).I * S (Curr_TCB.T + 1).I;

            when k_DIV_Integer =>
              if S (Curr_TCB.T + 1).I = 0 then
                PS := DIVCHK;
              else
                S (Curr_TCB.T).I := S (Curr_TCB.T).I / S (Curr_TCB.T + 1).I;
              end if;

            when k_Power_Integer =>
              S (Curr_TCB.T).I := S (Curr_TCB.T).I ** S (Curr_TCB.T + 1).I;

            when k_Power_Float_Integer =>
              S (Curr_TCB.T).R := S (Curr_TCB.T).R ** S (Curr_TCB.T + 1).I;

            when k_Power_Float =>
              S (Curr_TCB.T).R := S (Curr_TCB.T).R ** S (Curr_TCB.T + 1).R;

            when k_MOD_Integer =>
              if S (Curr_TCB.T + 1).I = 0 then
                PS := DIVCHK;
              else
                S (Curr_TCB.T).I := S (Curr_TCB.T).I mod S (Curr_TCB.T + 1).I;
              end if;

            when k_MULT_Float =>
              S (Curr_TCB.T).R := S (Curr_TCB.T).R * S (Curr_TCB.T + 1).R;

            when k_DIV_Float =>
              S (Curr_TCB.T).R := S (Curr_TCB.T).R / S (Curr_TCB.T + 1).R;

            when others =>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;

        when kGetNewline =>
          if FAT.CURR = 0 then       --  Schoening
            if End_Of_File_Console then
              PS := REDCHK;
            else
              Skip_Line_Console;
            end if;
          elsif Ada.Text_IO.End_Of_File (FAT.FIL (FAT.CURR)) then
            PS := REDCHK;
          else
            Ada.Text_IO.Skip_Line (FAT.FIL (FAT.CURR));
          end if;
          SWITCH := True;  --  give up control when doing I/O

        when kPutNewline =>
          if FAT.CURR = 0 then      --  Schoening
            New_Line_Console;
          else
            Ada.Text_IO.New_Line (FAT.FIL (FAT.CURR));
          end if;
          SWITCH := True;  --  give up control when doing I/O

        when k_Set_current_file_pointer =>
          FAT.CURR := IR.Y;

        when kFile_I_O =>
          --  File I/O procedures - Schoening
          case IR.Y is
            when 7 =>
              if Ada.Text_IO.Is_Open (FAT.FIL (IR.X)) then
                Ada.Text_IO.Close (FAT.FIL (IR.X));   --  just in case
              end if;
              H1 := 0; -- was IOresult ;   --  clears any I/O error
              Ada.Text_IO.Open (FAT.FIL (IR.X), Ada.Text_IO.In_File, FAT.NAM (IR.X) & ".DAT");

            when 8 =>
              if Ada.Text_IO.Is_Open (FAT.FIL (IR.X)) then
                Ada.Text_IO.Close (FAT.FIL (IR.X));   --  just in case
              end if;
              H1 := 0; -- was IOresult ;   --  clears any I/O error
              Ada.Text_IO.Create (FAT.FIL (IR.X), Ada.Text_IO.Out_File, FAT.NAM (IR.X) & ".DAT");

            when 9 =>
              Ada.Text_IO.Close (FAT.FIL (IR.X));    --  close file

            when others =>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;
          --  case IR.Y
          SWITCH := True;  --  give up control when doing I/O

        when k_Halt_Interpreter =>
          if TActive = 0 then
            PS := FIN;
          else
            TCB (0).TS     := Completed;
            SWITCH         := True;
            Curr_TCB.PC := Curr_TCB.PC - 1;
          end if;

        when k_String_assignment =>
          --  Hathorn
          H1 := S (Curr_TCB.T - 2).I;  --  address of array
          H2 := S (Curr_TCB.T).I;    --  pointer to string table
          H3 := IR.Y;      --  size of array
          H4 := S (Curr_TCB.T - 1).I;  --  length of string
          if H3 < H4 then
            H5 := H1 + H3;    --  H5 is H1 + min of H3, H4
          else
            H5 := H1 + H4;
          end if;
          while H1 < H5 loop
            --  copy H5-H1 characters
            S (H1).I := Character'Pos (HAC.Data.StringTab (H2));
            H1       := H1 + 1;
            H2       := H2 + 1;
          end loop;
          H5 := S (Curr_TCB.T - 2).I + H3;              --  H5 = H1 + H3
          while H1 < H5 loop
            --  fill with blanks if req'd
            S (H1).I := Character'Pos (' ');
            H1       := H1 + 1;
          end loop;
          Curr_TCB.T := Curr_TCB.T - 3;

        when k_Delay =>  --  DLY - delay for a specified number of seconds

          if S (Curr_TCB.T).R > 0.0 then
            --  if positive delay time

            Curr_TCB.TS := Delayed;           --  set task state to delayed

            SYSCLOCK := GetClock;    --  update System Clock

            Curr_TCB.WAKETIME := SYSCLOCK +
                                    Duration (S (Curr_TCB.T).R * 1000.0);
            --  set wakeup time

            --  internal time units is milliseconds so X 1000.0

            SWITCH := True;          --  give up control

          end if;
          Curr_TCB.T := Curr_TCB.T - 1;
        --  Delay

        when k_Cursor_At =>
          --  Cramer
          H2         := S (Curr_TCB.T - 1).I;  --  row
          H1         := S (Curr_TCB.T).I;      --  column
          Curr_TCB.T := Curr_TCB.T - 2; --  issue TPC call
        -- GotoXY (H1, H2);

        when k_Set_Quantum_Task =>
          --  Cramer
          if S (Curr_TCB.T).R <= 0.0 then
            S (Curr_TCB.T).R := HAC.Data.HAC_Float (HAC.Data.TSlice) * 0.001;
          end if;
          TCB (CurTask).QUANTUM := Duration (S (Curr_TCB.T).R);
          Curr_TCB.T         := Curr_TCB.T - 1;

        when k_Set_Task_Priority =>
          --  Cramer
          if S (Curr_TCB.T).I > HAC.Data.PriMax then
            S (Curr_TCB.T).I := HAC.Data.PriMax;
          end if;
          if S (Curr_TCB.T).I < 0 then
            S (Curr_TCB.T).I := 0;
          end if;
          TCB (CurTask).Pcontrol.UPRI := S (Curr_TCB.T).I;
          Curr_TCB.T                  := Curr_TCB.T - 1;

        when k_Set_Task_Priority_Inheritance =>
          --  Cramer
          Curr_TCB.Pcontrol.INHERIT := S (Curr_TCB.T).I mod 2 = 1;
          --  Set priority inherit indicator
          Curr_TCB.T := Curr_TCB.T - 1;

        when k_Selective_Wait =>
          --  Selective Wait Macro Instruction

          case IR.X is
            when 1 => --  Start Selective Wait seq.
              TCB (CurTask).R1.I := 0; --  next instruction if delay expires
              TCB (CurTask).R2.R := -1.0; --  delay time

            when 2 => --  Retain entry ID
              TCB (CurTask).R3.I := IR.Y;

            when 3 => --  Accept if its still on queue
              H1 := TCB (CurTask).R3.I;
              H2 := FirstCaller (H1);    --  first waiting task
              H3 := HAC.Data.IdTab (H1).LEV;        --  level of accepting entry
              if H2 >= 0 then
                Curr_TCB.DISPLAY (H3 + 1) := TCB (H2).B; --  address
                                                            --callers parms
                Curr_TCB.InRendzv := H2;             --  indicate task
                                                        --InRendz
                if TCB (H2).TS = TimedRendz then --  turn off entry timeout
                  TCB (H2).TS := WaitRendzv;    --  if it was on
                end if;
              else
                Curr_TCB.PC := IR.Y; --  Jump to patched in address
              end if;
              SWITCH := True;

            when 4 => --  Update minimum delay time
              if S (Curr_TCB.T).R > 0.0 then
                if TCB (CurTask).R2.R = -1.0 then
                  TCB (CurTask).R2.R := S (Curr_TCB.T).R;
                  TCB (CurTask).R1.I := IR.Y;   --  ins after JMP
                else
                  if S (Curr_TCB.T).R < TCB (CurTask).R2.R then
                    TCB (CurTask).R2.R := S (Curr_TCB.T).R;
                    TCB (CurTask).R1.I := IR.Y;   --  ins after JMP
                  end if;
                end if;
              end if;
              Curr_TCB.T := Curr_TCB.T - 1;

            when 5 | 6 => --  end of SELECT

              if TCB (CurTask).R2.R > 0.0 then
                --  Timed Wait
                Curr_TCB.TS       := TimedWait;
                SYSCLOCK             := GetClock;
                Curr_TCB.WAKETIME := SYSCLOCK +
                                        Duration (TCB (CurTask).R2.R *
                                                  1000.0);
                Curr_TCB.PC       := IR.Y; --  Do SELECT again when
                                              --awakened by caller

                SWITCH := True;                     --  give up control

              end if;
              --  AVL -- TERMINATE
              --  IS THE PARENT TASK COMPLETED?
              if TCB (0).TS = Completed and CurTask /= 0 and IR.X /= 6 then
                NCALLS := 0; --  LET'S SEE IF THERE ARE CALLERS
                for ITERM in 1 .. HAC.Data.ECount loop
                  if EList (ITERM).First /= null then
                    NCALLS := NCALLS + 1;
                  end if;
                end loop;
                --  YES, NO CALLERS
                if NCALLS = 0 then  --  YES, NO CALLERS
                  --  ARE THE SIBLING TASKS EITHER COMPLETED OR
                  --  IN THE SAME STATE AS CURTASK?
                  NCOMPL := 0;
                  for ITERM in 1 .. HAC.Data.TCount loop
                    if TCB (ITERM).TS = Completed then
                      NCOMPL := NCOMPL + 1;
                    else
                      if TCB (ITERM).TS = TCB (CurTask).TS then
                        NCOMPL := NCOMPL + 1;
                      else
                        if TCB (ITERM).TS = Ready and
                           TCB (CurTask).TS = Running
                        then
                          NCOMPL := NCOMPL + 1;
                        end if;
                      end if;
                    end if;
                  end loop;
                  if HAC.Data.TCount = NCOMPL then
                    --  YES, THEN ALL TASKS ARE NOW TERMINATING
                    for ITERM in 1 .. HAC.Data.TCount loop
                      TCB (ITERM).TS := Terminated;
                    end loop;
                    PS := FIN;
                  end if;
                end if;
              end if;
            --               if ir.x = 6 then
            --               begin
            --                 term := false ;    {Task doesn't have a
            --terminate}
            --               end ;                {alternative}
            --

            when others =>
              null;  -- [P2Ada]: no otherwise / else in Pascal
          end case;

        --  Selective Wait

        when k_Switch_2 =>
          null;
        when k_NOP =>
          null;
        when kHighlightSource =>
          null;
        end case;
        --  main case IR.F
      end;
      exit when InterDef.PS /= InterDef.RUN;
    end loop;
    <<LABEL_123777>>
    if InterDef.PS /= InterDef.FIN then
      Post_Mortem_Dump;
    end if;
    --  begin
    --  --  GotoXY (1, 20) ;
    --
    --    New_Line;
    --    Put ("Program terminated normally ...");
    --    New_Line;
    --  end;

  end Interpret;

  procedure Interpret_on_Current_IO_Instance is new Interpret
    ( Ada.Text_IO.End_Of_File,
      Ada.Text_IO.End_Of_Line,
      Ada.Integer_Text_IO.Get,
      RIO.Get,
      Ada.Text_IO.Get,
      Ada.Text_IO.Skip_Line,
      Ada.Integer_Text_IO.Put,
      RIO.Put,
      Boolean_Text_IO.Put,
      Ada.Text_IO.Put,
      Ada.Text_IO.Put,
      Ada.Text_IO.New_Line);

  procedure Interpret_on_Current_IO renames Interpret_on_Current_IO_Instance;

end HAC.PCode.Interpreter;
-- Translated on 23-Jan-2013 by (New) P2Ada v. 28-Oct-2009
