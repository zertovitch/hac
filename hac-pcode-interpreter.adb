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

    type Processor_State is (
      RUN,     --  RUN is the normal processor state
      --
      Case_Check_Error,
      -- ConstErr,
      DEADLOCK,
      DIVCHK,
      FIN,
      INXCHK,  --  Out-of-range error
      -- NumErr  ,
      ProgErr,
      REDCHK,
      STKCHK,  --  Stack overflow state
      -- StoErr  ,
      -- TaskErr ,
      WAIT);

    type Task_State is (
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

    FAT : FilDescr;                     --  File i/o table

    type Display_type is array (1 .. HAC.Data.LMax) of Integer;

    type Task_Control_Block is record   --  Task Control Block
      --  index of current top of stack
      T : Integer;
      --  index of current base of stack
      B : Integer;
      --  program counter, next pcode
      PC : Integer;
      --  current task state
      TS : Task_State;
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
      Task_Index : TRange;  --  index of task enqueued for rendzv
      Next : Eptr;  --  next entry in list
    end record;

    procedure Dispose is new Ada.Unchecked_Deallocation (Enode, Eptr);

    type EHeader is record
      Task_Index : TRange;  --  index of task that contains entry
      First : Eptr;  --  ptr to first node in rendzv queue
      Last : Eptr;  --  ptr to last  node in rendzv queue
    end record;

    IR       : Order;  --  Instruction register

    PS       : Processor_State := RUN;    --  Processor status register

    TCB      : array (TRange) of Task_Control_Block;  --  Task control blocks
    EList    : array (1 .. HAC.Data.EntryMax) of EHeader; --  Entry queue array
    SWITCH   : Boolean:= False; --  invoke scheduler on next cycle flag
    SYSCLOCK : Time:= Clock;    --  (ms after 00:00:00 Jan 1, current year)
    TIMER    : Time:= SYSCLOCK; --  set to end of current task's time slice
    TActive  : TRange;  --  no. of active tasks

    CurTask : Integer;  --  index of currently executing task

    H1, H2, H3, H4, H5 : Integer;  --  Internal integer registers
    F1     : HAC.Data.HAC_Float;   --  Internal float registers
    BLKCNT : Integer;

    NCALLS : Integer;   --  AVL  TERMINATE
    NCOMPL : Integer;   --  AVL  TERMINATE

    function GetClock return Time renames Clock;

    SNAP: Boolean;  --  SNAP-shot flag To Display sched. status

  end InterDef;

  --  Post Mortem Dump of the task stack causing the exception
  --
  procedure Post_Mortem_Dump (CD: HAC.Compiler.Compiler_Data) is
    use HAC.Compiler, InterDef;
    use Ada.Text_IO, Ada.Integer_Text_IO, Boolean_Text_IO;
  begin
      New_Line;
      Put_Line ("HAC - PCode - Post Mortem Dump");
      New_Line;
      Put_Line ("Processor state: " & Processor_State'Image (InterDef.PS));
      New_Line;
      Put_Line ("Stack Variables of Task " & CD.IdTab (CD.Tasks_Definitions_Table (InterDef.CurTask)).Name);
      InterDef.H1 := InterDef.TCB (InterDef.CurTask).B;   --  current bottom of stack
      InterDef.BLKCNT := 10;
      loop
        New_Line;
        InterDef.BLKCNT := InterDef.BLKCNT - 1;
        if InterDef.BLKCNT = 0 then
          InterDef.H1 := 0;
        end if;
        InterDef.H2 := S (InterDef.H1 + 4).I;  --  index into HAC.Data.IdTab for this process
        if InterDef.H1 /= 0 then
          Put (CD.IdTab (InterDef.H2).Name);
          Put (" CALLED AT");
          Put (S (InterDef.H1 + 1).I, 5);
          New_Line;
        else
          Put_Line ("Task Variables");
        end if;
        InterDef.H2 := CD.Blocks_Table (CD.IdTab (InterDef.H2).Ref).Last;
        while InterDef.H2 /= 0 loop
          -- [P2Ada]: WITH instruction
          declare
            P2Ada_Var_7 : IdTabEntry renames CD.IdTab (InterDef.H2);
            use HAC.Data;
          begin
            if P2Ada_Var_7.Obj = Variable then
              if HAC.Data.Standard_or_Enum_Typ (P2Ada_Var_7.TYP) then
                if P2Ada_Var_7.Normal then
                  InterDef.H3 := InterDef.H1 + P2Ada_Var_7.Adr;
                else
                  InterDef.H3 := InterDef.S (InterDef.H1 + P2Ada_Var_7.Adr).I;
                end if;
                Put ("  " & P2Ada_Var_7.Name & " = ");
                case P2Ada_Var_7.TYP is
                  when HAC.Data.Enums | HAC.Data.Ints =>
                    Put (S (H3).I);
                    New_Line;
                  when HAC.Data.Bools =>
                    Put (Boolean'Val (S (H3).I));
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

  procedure Interpret (CD: HAC.Compiler.Compiler_Data)
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
      while t <= CD.Tasks_Definitions_Count and not taskdelayed loop
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
      while i <= CD.Entries_Count and e = -1 loop
        if Entry_Index = CD.Entries_Table (i) then
          e := i;
        end if;
        i := i + 1;
      end loop;
      return e;
    end EIndex;

    procedure Queue (Entry_Index : Integer; CallingTask: InterDef.TRange) is
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
      Put (CD.IdTab (Entry_Index).Name);
      Put (" entry index=");
      Put (ix);
      New_Line;
      if p /= null then
        loop
          Put ("Task ");
          Put (CD.IdTab (CD.Tasks_Definitions_Table (p.Task_Index)).Name);
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
      for t in 0 .. CD.Tasks_Definitions_Count loop
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

    --  Default Task time-slice in milliseconds
    --  Feldman: 60ths of a sec on Mac
    TSlice : constant := 16.666666;

    procedure Init_main_task is
    begin
      Reset(Gen); --  initialize TPC random number generator
      --  After compiled, just begin exec
      --  Initialize run-time stack
      S (1).I := 0 ;
      S (2).I := 0 ;
      S (3).I := -1 ;
      S (4).I := CD.Tasks_Definitions_Table (0) ;
      declare
        Main_TCB : Task_Control_Block renames TCB(0);
      begin
        Main_TCB.PC := CD.IdTab (CD.Tasks_Definitions_Table (0)).Adr ; --  first pcode instruction
        Main_TCB.T := CD.Blocks_Table (1).VSize - 1 ; -- was CD.Blocks_Table (2)
        Main_TCB.B := 0 ;
        Main_TCB.TS := Ready ;
        Main_TCB.InRendzv := NilTask ;
        Main_TCB.DISPLAY (1) := 0 ;
        Main_TCB.STACKSIZE := HAC.Data.StMax - (CD.Tasks_Definitions_Count * HAC.Data.STKINCR) ;
        Main_TCB.SUSPEND := 0 ;
        Main_TCB.QUANTUM := Duration (TSlice * 0.001);
        Main_TCB.Pcontrol.UPRI := 0 ;
        Main_TCB.Pcontrol.INHERIT := False ;
        Main_TCB.LASTRUN := Start_Time ;
      end;
    end Init_main_task;

    procedure Init_other_tasks is
    begin
      for CurTask in 1 .. CD.Tasks_Definitions_Count loop
        declare
          Curr_TCB : Task_Control_Block renames TCB(CurTask);
        begin
          H1 := CD.Tasks_Definitions_Table (CurTask) ;
          Curr_TCB.PC := CD.IdTab (H1).Adr ;
          Curr_TCB.B := TCB (CurTask - 1).STACKSIZE + 1 ;
          Curr_TCB.T := Curr_TCB.B + CD.Blocks_Table (CD.IdTab (H1).Ref).VSize - 1 ;
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
          Curr_TCB.QUANTUM := Duration (TSlice * 0.001);
          Curr_TCB.Pcontrol.UPRI := 0 ;
          Curr_TCB.Pcontrol.INHERIT := False ;
          Curr_TCB.LASTRUN := Start_Time ;
        end;
      end loop;
      --  Initially no queued entry calls
      for H1 in 1 .. CD.Entries_Count loop
        EList (H1).Task_Index := CD.IdTab (CD.Entries_Table (H1)).Adr ;  --  Task index
        EList (H1).First := null ;
        EList (H1).Last  := null ;
      end loop;
      TActive := CD.Tasks_Definitions_Count ;  --  All tasks are active initially
      CurTask := 0 ;  --  IT WAS -1 ?
      SWITCH := True ;
      TIMER := Start_Time; -- was 0.0
      PS := RUN ;
    end Init_other_tasks;

    procedure ShowTime is null;
    procedure SnapShot is null;

  begin  --  Interpret
    InterDef.SNAP:= False;
    Init_main_task;
    Init_other_tasks;

    Running_State:
    loop  --  until Processor state /= RUN
      SYSCLOCK := GetClock;
      if InterDef.SNAP then
        ShowTime ;
      end if;
      if InterDef.TCB (InterDef.CurTask).TS = InterDef.Critical then
        if InterDef.SNAP then
          SnapShot ;
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
              ShowTime ;
            end if;
            if InterDef.SNAP then
              SnapShot ;
            end if;
            exit when PS /= WAIT;
          end loop;
          --
          exit Running_State when PS = DEADLOCK or PS = FIN;
          --
          TIMER:= InterDef.SYSCLOCK + InterDef.TCB (InterDef.CurTask).QUANTUM;
          InterDef.TCB (InterDef.CurTask).TS := Running;
          SWITCH:= False;
          if InterDef.SNAP then
            SnapShot ;
          end if;
        end if;
      end if;

      --  FETCH INSTRUCTION
      declare
        Curr_TCB : InterDef.Task_Control_Block renames InterDef.TCB (InterDef.CurTask);
      begin
        InterDef.IR := CD.ObjCode (Curr_TCB.PC);
        Curr_TCB.PC := Curr_TCB.PC + 1;
      end;

      --  HERE IS THE POINT WHERE THE TASK MONITORING IS CALLED
      --  (removed)

      declare
        Curr_TCB : InterDef.Task_Control_Block renames InterDef.TCB (InterDef.CurTask);
        CH : Character;
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
            H3 := CD.IdTab (H1).LEV;            --  level of accepting entry
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
            H1         := S (Curr_TCB.T).I;
            Curr_TCB.T := Curr_TCB.T - 1;
            H2         := CD.Tasks_Definitions_Count + 1;
            H3         := Integer (Random (Gen) * Float (H2));
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
              when SF_Abs =>
                S (Curr_TCB.T).I := abs (S (Curr_TCB.T).I);
              when SF_Abs + 1 =>
                S (Curr_TCB.T).R := abs (S (Curr_TCB.T).R);
              when SF_T_Val =>   --  S'Val : RM 3.5.5 (5)
                if (S (Curr_TCB.T).I < HAC.Data.OrdMinChar) or
                  (S (Curr_TCB.T).I > HAC.Data.OrdMaxChar)  --  !! Character range
                then
                  PS := INXCHK;  --  Seems an out-of-range
                end if;
              when SF_T_Pos =>   --  S'Pos : RM 3.5.5 (2)
                null;
              when SF_T_Succ =>  --  S'Succ : RM 3.5 (22)
                S (Curr_TCB.T).I := S (Curr_TCB.T).I + 1;
              when SF_T_Pred =>  --  S'Pred : RM 3.5 (25)
                S (Curr_TCB.T).I := S (Curr_TCB.T).I - 1;
              when SF_Round_Float_to_Int =>
                S (Curr_TCB.T).I := Integer (S (Curr_TCB.T).R);
              when SF_Trunc_Float_to_Int =>
                S (Curr_TCB.T).I := Integer (HAC.Data.HAC_Float'Floor (S (Curr_TCB.T).R));
              when SF_Sin =>
                S (Curr_TCB.T).R := Sin (S (Curr_TCB.T).R);
              when SF_Cos =>
                S (Curr_TCB.T).R := Cos (S (Curr_TCB.T).R);
              when SF_Exp =>
                S (Curr_TCB.T).R := Exp (S (Curr_TCB.T).R);
              when SF_Log =>
                S (Curr_TCB.T).R := Log (S (Curr_TCB.T).R);
              when SF_Sqrt =>
                S (Curr_TCB.T).R := Sqrt (S (Curr_TCB.T).R);
              when SF_Arctan =>
                S (Curr_TCB.T).R := Arctan (S (Curr_TCB.T).R);
              when SF_EOF =>
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
              when SF_EOLN =>
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
              when SF_Random =>
                S (Curr_TCB.T).I :=
                  Integer (HAC.Data.HAC_Float (Random (Gen)) *
                           HAC.Data.HAC_Float ((S (Curr_TCB.T).I + 1)));
              when SF_Clock =>
                --  CLOCK function, NILADIC functions have IR.Y => SF_Clock.
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

        when k_Record_Field_Offset =>
          S (Curr_TCB.T).I := S (Curr_TCB.T).I + IR.Y;

        when k_Jump =>
          Curr_TCB.PC := IR.Y;

        when k_Conditional_Jump =>
          if S (Curr_TCB.T).I = 0 then  --  False
            Curr_TCB.PC := IR.Y;        --  Jump
          end if;
          Curr_TCB.T := Curr_TCB.T - 1;

        when k_CASE_Switch_1 =>  --  SWTC - switch (in a CASE instruction)
          H1         := S (Curr_TCB.T).I;
          Curr_TCB.T := Curr_TCB.T - 1;
          H2         := IR.Y;
          --
          --  Now we loop over a bunch of k_CASE_Switch_2 instruction pairs that covers all cases.
          --
          loop
            if CD.ObjCode (H2).F /= k_CASE_Switch_2 then
              PS := Case_Check_Error;  --  Value or OTHERS not found. This situation should not...
              exit;                    --  ...happen: compiler should check it before run-time.
            elsif CD.ObjCode (H2).Y = H1    --  either: - value is matching
                  or CD.ObjCode (H2).X < 0  --      or: - "WHEN OTHERS =>" case
            then
              Curr_TCB.PC := CD.ObjCode (H2 + 1).Y;  --  Execute instructions after "=>".
              exit;
            else
              H2 := H2 + 2;  --  Check the next k_CASE_Switch_2 instruction pair.
            end if;
          end loop;

        when k_CASE_Switch_2 =>
          --  This instruction appears only in a special object code block, see k_CASE_Switch_1.
          null;

        when k_FOR_Forward_Begin =>  --  Start of a FOR loop, forward direction
          H1 := S (Curr_TCB.T - 1).I;
          if H1 <= S (Curr_TCB.T).I then
            S (S (Curr_TCB.T - 2).I).I := H1;
          else
            Curr_TCB.T  := Curr_TCB.T - 3;
            Curr_TCB.PC := IR.Y;
          end if;

        when k_FOR_Forward_End =>  --  End of a FOR loop, forward direction
          H2 := S (Curr_TCB.T - 2).I;
          H1 := S (H2).I + 1;
          if H1 <= S (Curr_TCB.T).I then
            S (H2).I       := H1;
            Curr_TCB.PC := IR.Y;
          else
            Curr_TCB.T := Curr_TCB.T - 3;
          end if;

        when k_FOR_Reverse_Begin =>  --  Start of a FOR loop, reverse direction
          H1 := S (Curr_TCB.T).I;
          if H1 >= S (Curr_TCB.T - 1).I then
            S (S (Curr_TCB.T - 2).I).I := H1;
          else
            Curr_TCB.PC := IR.Y;
            Curr_TCB.T  := Curr_TCB.T - 3;
          end if;

        when k_FOR_Reverse_End =>  --  End of a FOR loop, reverse direction
          H2 := S (Curr_TCB.T - 2).I;
          H1 := S (H2).I - 1;
          if H1 >= S (Curr_TCB.T - 1).I then
            S (H2).I       := H1;
            Curr_TCB.PC := IR.Y;
          else
            Curr_TCB.T := Curr_TCB.T - 3;
          end if;

        when k_Mark_Stack =>
          H1 := CD.Blocks_Table (CD.IdTab (IR.Y).Ref).VSize;
          if Curr_TCB.T + H1 > Curr_TCB.STACKSIZE then
            PS := STKCHK;
          else
            Curr_TCB.T := Curr_TCB.T + 5; --  make room for fixed area
            S (Curr_TCB.T - 1).I := H1 - 1; --  vsize-1
            S (Curr_TCB.T).I := IR.Y;    --  HAC.Data.IdTab index of called
                                            --procedure/entry
          end if;

        when k_Call =>
          --  procedure and task entry CALL
          --  Cramer
          if IR.X = HAC.Data.CallTMDE then
            --  Timed entry call
            F1 := S (Curr_TCB.T).R;  --  Pop delay time
            Curr_TCB.T := Curr_TCB.T - 1;
          end if;
          H1 := Curr_TCB.T - IR.Y;     --  base of activation record
          H2 := S (H1 + 4).I;          --  CD.IdTab index of called procedure/entry
          H3                        := CD.IdTab (H2).LEV;
          Curr_TCB.DISPLAY (H3 + 1) := H1;
          S (H1 + 1).I              := Curr_TCB.PC;  --  return address

          H4 := S (H1 + 3).I + H1; --  new top of stack
          S (H1 + 2).I := Curr_TCB.DISPLAY (H3); --  static link
          S (H1 + 3).I := Curr_TCB.B; --  dynamic link

          for H3 in Curr_TCB.T + 1 .. H4 loop
            S (H3).I := 0;  --  initialize local vars
          end loop;
          Curr_TCB.B := H1;
          Curr_TCB.T := H4;
          case IR.X is
            when  --  Call type
             HAC.Data.CallSTDP =>
              --  Standard procedure call

              Curr_TCB.PC := CD.IdTab (H2).Adr;

            when HAC.Data.CallSTDE =>
              --  Unconditional entry call
              Queue (H2, CurTask);          --  put self on entry queue
              Curr_TCB.TS := WaitRendzv;
              H5          := CD.IdTab (H2).Adr;  --  Task being entered
              if ((TCB (H5).TS = WaitRendzv) and (TCB (H5).SUSPEND = H2)) or
                 (TCB (H5).TS = TimedWait)
              then
                --  wake accepting task if necessary
                TCB (H5).TS      := Ready;
                TCB (H5).SUSPEND := 0;
              end if;
              SWITCH := True;                 --  give up control

            when HAC.Data.CallTMDE =>
              --  Timed entry call
              Queue (H2, CurTask);      --  put self on entry queue
              H5 := CD.IdTab (H2).Adr;  --  Task being entered
              --
              if ((TCB (H5).TS = WaitRendzv) and (TCB (H5).SUSPEND = H2)) or
                 (TCB (H5).TS = TimedWait)
              then
                --  wake accepting task if necessary
                Curr_TCB.TS := WaitRendzv;     --  suspend self
                TCB (H5).TS := Ready;          --  wake accepting task
                TCB (H5).SUSPEND := 0;
              else
                Curr_TCB.TS := TimedRendz;     --  Timed Wait For Rendezvous
                Curr_TCB.R1.I := 1;            --  Init R1 to specify NO timeout
                Curr_TCB.R2.I := H2;           --  Save address of queue for purge
                SYSCLOCK := GetClock; --  update System Clock
                Curr_TCB.WAKETIME := SYSCLOCK + Duration (F1 * 1000.0);
                --  internal time units is milliseconds so X 1000.0
              end if;
              SWITCH := True;       --  give up control

            when HAC.Data.CallCNDE =>
              --  Conditional Entry Call
              H5 := CD.IdTab (H2).Adr;              --  Task being entered
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

        when k_Array_Index_Element_Size_1 =>
          H1 := IR.Y;     --  H1 points to HAC.Data.Arrays_Table
          H2 := CD.Arrays_Table (H1).Low;
          H3 := S (Curr_TCB.T).I;
          if H3 not in H2 .. CD.Arrays_Table (H1).High then
            PS := INXCHK;  --  Out-of-range state
          else
            Curr_TCB.T       := Curr_TCB.T - 1;
            S (Curr_TCB.T).I := S (Curr_TCB.T).I + (H3 - H2);
          end if;

        when k_Array_Index =>
          H1 := IR.Y;      --  H1 POINTS TO HAC.Data.Arrays_Table
          H2 := CD.Arrays_Table (H1).Low;
          H3 := S (Curr_TCB.T).I;
          if H3 not in H2 .. CD.Arrays_Table (H1).High then
            PS := INXCHK;  --  Out-of-range state
          else
            Curr_TCB.T       := Curr_TCB.T - 1;
            S (Curr_TCB.T).I := S (Curr_TCB.T).I +
                                   (H3 - H2) * CD.Arrays_Table (H1).ELSize;
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

        when k_Literal =>  --  Literal: discrete value (Integer, Character, Boolean, Enum)
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
            S (Curr_TCB.T).R := CD.Float_Constants_Table (IR.Y);
          end if;

        when k_Integer_to_Float =>
          H1       := Curr_TCB.T - IR.Y;
          S (H1).R := HAC.Data.HAC_Float (S (H1).I);

        when k_Read =>
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
                  Get_Console (CH);
                  S (S (Curr_TCB.T).I).I := Character'Pos (CH);
                when 4 =>
                  Get_Console (CH);
                  S (S (Curr_TCB.T).I).I := Character'Pos (CH);
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
                  Ada.Text_IO.Get (FAT.FIL (FAT.CURR), CH);
                  S (S (Curr_TCB.T).I).I := Character'Pos (CH);
                when 4 =>
                  Ada.Text_IO.Get (FAT.FIL (FAT.CURR), CH);
                  S (S (Curr_TCB.T).I).I := Character'Pos (CH);
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
              Put_Console (CD.Strings_Table (H2));
            else
              Ada.Text_IO.Put (FAT.FIL (FAT.CURR), CD.Strings_Table (H2));
            end if;
            H1 := H1 - 1;        --  decrement length
            H2 := H2 + 1;
            --  increment char pointer
          end loop;
          SWITCH := True;        --  give up control when doing I/O

        when k_Write_1 =>
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

        when k_Write_2 =>
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
              if IR.Y = HAC.Data.CallTMDE and Curr_TCB.R1.I = 0 then
                Curr_TCB.T := Curr_TCB.T + 1;         --  A JMPC
                                                      --  instruction
                                                      --  always follows
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

        when k_Case_34 =>
          S (Curr_TCB.T) := S (S (Curr_TCB.T).I);  --  "stack_top := (stack_top.I).all"

        when k_NOT_Boolean =>
          S (Curr_TCB.T).I := Boolean'Pos (not Boolean'Val (S (Curr_TCB.T).I));

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

        when Binary_Operator_Opcode =>
          Curr_TCB.T := Curr_TCB.T - 1;  --  [T] <- [T] operator [T + 1]
          declare
            X : GRegister renames S (Curr_TCB.T);
            Y : GRegister renames S (Curr_TCB.T + 1);
          begin
            case Binary_Operator_Opcode (IR.F) is
              when k_EQL_Float => X.I := Boolean'Pos (X.R = Y.R);
              when k_NEQ_Float => X.I := Boolean'Pos (X.R /= Y.R);
              when k_LSS_Float => X.I := Boolean'Pos (X.R < Y.R);
              when k_LEQ_Float => X.I := Boolean'Pos (X.R <= Y.R);
              when k_GTR_Float => X.I := Boolean'Pos (X.R > Y.R);
              when k_GEQ_Float => X.I := Boolean'Pos (X.R >= Y.R);
              --
              when k_EQL_Integer => X.I := Boolean'Pos (X.I = Y.I);
              when k_NEQ_Integer => X.I := Boolean'Pos (X.I /= Y.I);
              when k_LSS_Integer => X.I := Boolean'Pos (X.I < Y.I);
              when k_LEQ_Integer => X.I := Boolean'Pos (X.I <= Y.I);
              when k_GTR_Integer => X.I := Boolean'Pos (X.I > Y.I);
              when k_GEQ_Integer => X.I := Boolean'Pos (X.I >= Y.I);
              --
              when k_AND_Boolean => X.I := Boolean'Pos (Boolean'Val (X.I) and Boolean'Val (Y.I));
              when k_OR_Boolean  => X.I := Boolean'Pos (Boolean'Val (X.I) or  Boolean'Val (Y.I));
              when k_XOR_Boolean => X.I := Boolean'Pos (Boolean'Val (X.I) xor Boolean'Val (Y.I));
              --
              when k_ADD_Integer      => X.I := X.I + Y.I;
              when k_SUBTRACT_Integer => X.I := X.I - Y.I;
              when k_MULT_Integer     => X.I := X.I * Y.I;
              when k_DIV_Integer      => if Y.I = 0 then PS := DIVCHK; else X.I := X.I / Y.I; end if;
              when k_MOD_Integer      => if Y.I = 0 then PS := DIVCHK; else X.I := X.I mod Y.I; end if;
              when k_Power_Integer    => X.I := X.I ** Y.I;
              --
              when k_ADD_Float           => X.R := X.R + Y.R;
              when k_SUBTRACT_Float      => X.R := X.R - Y.R;
              when k_MULT_Float          => X.R := X.R * Y.R;
              when k_DIV_Float           => X.R := X.R / Y.R;
              when k_Power_Float         => X.R := X.R ** Y.R;
              when k_Power_Float_Integer => X.R := X.R ** Y.I;
            end case;
          end;

        when k_Get_Newline =>
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

        when k_Put_Newline =>
          if FAT.CURR = 0 then      --  Schoening
            New_Line_Console;
          else
            Ada.Text_IO.New_Line (FAT.FIL (FAT.CURR));
          end if;
          SWITCH := True;  --  give up control when doing I/O

        when k_Set_current_file_pointer =>
          FAT.CURR := IR.Y;

        when k_File_I_O =>
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
          H2 := S (Curr_TCB.T).I;      --  pointer to string table
          H3 := IR.Y;                  --  size of array
          H4 := S (Curr_TCB.T - 1).I;  --  length of string
          if H3 < H4 then
            H5 := H1 + H3;    --  H5 is H1 + min of H3, H4
          else
            H5 := H1 + H4;
          end if;
          while H1 < H5 loop
            --  Copy H5-H1 characters to the stack
            S (H1).I := Character'Pos (CD.Strings_Table (H2));
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
            S (Curr_TCB.T).R := HAC.Data.HAC_Float (TSlice) * 0.001;
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
          Curr_TCB.Pcontrol.INHERIT := S (Curr_TCB.T).I /= 0;
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
              H3 := CD.IdTab (H1).LEV;        --  level of accepting entry
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
                for ITERM in 1 .. CD.Entries_Count loop
                  if EList (ITERM).First /= null then
                    NCALLS := NCALLS + 1;
                  end if;
                end loop;
                --  YES, NO CALLERS
                if NCALLS = 0 then  --  YES, NO CALLERS
                  --  ARE THE SIBLING TASKS EITHER COMPLETED OR
                  --  IN THE SAME STATE AS CURTASK?
                  NCOMPL := 0;
                  for ITERM in 1 .. CD.Tasks_Definitions_Count loop
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
                  if CD.Tasks_Definitions_Count = NCOMPL then
                    --  YES, THEN ALL TASKS ARE NOW TERMINATING
                    for ITERM in 1 .. CD.Tasks_Definitions_Count loop
                      TCB (ITERM).TS := Terminated;
                    end loop;
                    PS := FIN;
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
          --  Selective Wait

        end case;
        --  main case IR.F
      end;
      exit when InterDef.PS /= InterDef.RUN;
    end loop Running_State;
    --
    if InterDef.PS /= InterDef.FIN then
      Post_Mortem_Dump (CD);
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

  procedure Interpret_on_Current_IO (CD: HAC.Compiler.Compiler_Data)
    renames Interpret_on_Current_IO_Instance;

end HAC.PCode.Interpreter;
-- Translated on 23-Jan-2013 by (New) P2Ada v. 28-Oct-2009
