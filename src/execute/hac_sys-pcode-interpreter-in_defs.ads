-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--
--  In_Defs: Internal Interpreter Definitions

with HAC_Sys.Co_Defs, HAC_Sys.Defs;

with Ada.Calendar,
     Ada.Containers.Vectors,
     Ada.Numerics.Float_Random,
     Ada.Unchecked_Deallocation;

package HAC_Sys.PCode.Interpreter.In_Defs is

  NilTask : constant := -1;

  subtype TRange is Integer range 0 .. Defs.TaskMax;  --  task index

  type Processor_State is (
    Running,           --  Normal processor state
    Exception_Raised,
    --
    FIN,
    DEADLOCK,
    WAIT);

  subtype Running_or_in_Exception is Processor_State range Running .. Exception_Raised;

  type Task_State is (
    --  SmallAda tasking stuff, not tested yet:
    Completed,
    Delayed,
    Ready,
    --  HAC is currently tested on this:
    Running,
    Exception_Raised,
    --  SmallAda tasking stuff, not tested yet:
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

  type File_Ptr is access Ada.Text_IO.File_Type;

  Abstract_Console : constant File_Ptr := null;

  type General_Register (Special : Defs.Typen := Defs.NOTYP) is record
    --  I is used for most uses: indices in the stack, Integers, Bools, Chars and Enums.
    I : Defs.HAC_Integer;
    case Special is  --  This part is variant to save place.
      when Defs.Floats     => R   : Defs.HAC_Float;
      when Defs.VStrings   => V   : HAT.VString;
      when Defs.Times      => Tim : Ada.Calendar.Time;
      when Defs.Durations  => Dur : Duration;
      when Defs.Text_Files => Txt : File_Ptr := Abstract_Console;
      when others          => null;
    end case;
  end record;

  GR_Abstract_Console : constant General_Register :=
    (Special => Defs.Text_Files,
     I       => 0,
     Txt     => Abstract_Console);

  function GR_Real (R : Defs.HAC_Float) return General_Register;
  function GR_Time (T : Ada.Calendar.Time) return General_Register;
  function GR_Duration (D : Duration) return General_Register;

  function GR_VString (S : String) return General_Register;
  function GR_VString (V : HAT.VString) return General_Register;

  subtype Data_Type is General_Register;

  type Stack_Type is array (1 .. Defs.StMax) of Data_Type;

  type Stack_Type_Access is access Stack_Type;

  type Task_Control_Block is record
    T              : Defs.Index;            --  index of current top of stack
    B              : Defs.Index;            --  index of current base of stack
    PC             : Defs.Index;            --  program counter, next pcode
    TS             : Task_State;            --  current task state
    InRendzv       : Integer;               --  task in rendz with or -1
    WAKETIME       : Ada.Calendar.Time;     --  end of delay period
    Pcontrol       : PriCB;                 --  task priority parameter rec.
    QUANTUM        : Duration;              --  time slice
    LASTRUN        : Ada.Calendar.Time;     --  time last run end (fairness)
    DISPLAY        : Co_Defs.Display_Type;  --  Stack base index per nesting level,
                                            --      used for addressing variables.
    STACKSIZE      : Defs.Index;            --  Stack overflow is raised if exceeded.
    SUSPEND        : Integer;               --  id of object suspended on
    R1, R2, R3     : General_Register;
    R_Temp         : General_Register;
    Exception_Info : Exception_Propagation_Data;
  end record;

  type Enode;
  type Eptr is access Enode;  --  task entry rendzv pointer

  type Enode is record    --  task entry structure
    Task_Index : TRange;  --  index of task enqueued for rendzv
    Next       : Eptr;    --  next entry in list
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation (Enode, Eptr);

  type EHeader is record
    Task_Index : TRange;  --  index of task that contains entry
    First : Eptr;  --  ptr to first node in rendzv queue
    Last : Eptr;   --  ptr to last  node in rendzv queue
  end record;

  type Entry_Queue is array (1 .. Defs.EntryMax) of EHeader;

  package File_Vectors is new Ada.Containers.Vectors (Positive, File_Ptr);

  type Task_Control_Blocks is array (TRange) of Task_Control_Block;

  Single_Task : constant := -1;

  subtype Scheduler_Type is Integer range Single_Task .. 6;

  type Tick_Type is mod 128;

  --  Objects of type Interpreter_Data contains data that may be useful
  --  to be kept post-mortem, or in a snapshot toward the "outside", or
  --  passed to the scheduler.

  type Interpreter_Data is record
    S                         : Stack_Type_Access;
    PS                        : Processor_State;             --  Processor status register
    IR                        : Order;                       --  Instruction register
    CurTask                   : Integer;                     --  Index of currently executing task
    TCB                       : Task_Control_Blocks;
    Files                     : File_Vectors.Vector;
    Snap                      : Boolean;            --  Snapshot flag to display scheduler status
    Nb_Callers                : Integer;            --  AVL  TERMINATE
    Nb_Complete               : Integer;            --  AVL  TERMINATE
    EList                     : Entry_Queue;
    TActive                   : TRange;             --  no. of active tasks
    Start_Time                : Ada.Calendar.Time;
    SWITCH                    : Boolean;            --  invoke scheduler on next cycle flag
    SYSCLOCK                  : Ada.Calendar.Time;  --  (ms after 00:00:00 Jan 1, current year)
    TIMER                     : Ada.Calendar.Time;  --  set to end of current task's time slice
    Gen                       : Ada.Numerics.Float_Random.Generator;
    Scheduler                 : Scheduler_Type := Single_Task;
    Single_Task_Delay_Pending : Boolean        := False;
    Instr_Tick                : Tick_Type;
  end record;

  procedure Allocate_Text_File (
    ND : in out Interpreter_Data;
    R  : in out General_Register
  );

  procedure Free_Allocated_Contents (
    ND         : in out Interpreter_Data;
    Open_Files :    out Open_Files_Vectors.Vector
  );

  --  We have an "array of Character" (cf Is_Char_Array) on the stack
  function Get_String_from_Stack (ND : Interpreter_Data; Idx, Size : Integer) return String;

  procedure Pop (ND : in out Interpreter_Data; Amount : Positive := 1);
  pragma Inline (Pop);
  procedure Push (ND : in out Interpreter_Data; Amount : Positive := 1);
  pragma Inline (Push);

  VM_Case_Check_Error            : exception;
  VM_Division_by_0               : exception;
  VM_End_Error                   : exception;
  VM_Function_End_without_Return : exception;
  VM_Invalid_Data                : exception;
  VM_Out_of_Range                : exception;
  VM_Overflow_Error              : exception;
  VM_Raised_Exception            : exception;  --  See Name_Error for an example.
  VM_Stack_Overflow              : exception;
  VM_Stack_Underflow             : exception;
  VM_Subprogram_Spec             : exception;

  --  Post Mortem Dump of the task stack causing the exception
  --
  procedure Post_Mortem_Dump (CD : Co_Defs.Compiler_Data; ND : In_Defs.Interpreter_Data);

  procedure Check_Discriminant_Type (X : General_Register; Y : Defs.Typen);
  pragma Inline (Check_Discriminant_Type);

end HAC_Sys.PCode.Interpreter.In_Defs;
