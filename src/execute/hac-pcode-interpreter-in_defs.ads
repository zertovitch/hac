with HAC.Co_Defs;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;
with HAC.Defs;

package HAC.PCode.Interpreter.In_Defs is

  NilTask : constant := -1;

  subtype TRange is Integer range 0 .. Defs.TaskMax;  --  task index

  type Processor_State is (
    Running,           --  Normal processor state
    Exception_Raised,
    --
    FIN,
    DEADLOCK,
    WAIT);

  type Task_State is (
    Completed,
    Delayed,
    Ready,
    Running,
    Exception_Raised,
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

  type GRegister is record
    --  General register - variant record in Pascal
    --  !! To save place we'll reintroduce a discriminant
    --     - and use aux variables for conversions in the interpreter.
    --
    I   : Defs.HAC_Integer;  --  Also used for Bools, Chars and Enums.
    R   : Defs.HAC_Float;
    V   : Defs.VString;  --  !! might make copies slow (would a discriminant help?)
    Txt : File_Ptr := Abstract_Console;
  end record;

  subtype Data_Type is GRegister;

  type Stack_Type is array (1 .. Defs.StMax) of Data_Type;

  type Task_Control_Block is record
    T              : Integer;               --  index of current top of stack
    B              : Integer;               --  index of current base of stack
    PC             : Integer;               --  program counter, next pcode
    TS             : Task_State;            --  current task state
    InRendzv       : Integer;               --  task in rendz with or -1
    WAKETIME       : Time;                  --  end of delay period
    Pcontrol       : PriCB;                 --  task priority parameter rec.
    QUANTUM        : Duration;              --  time slice
    LASTRUN        : Time;                  --  time last run end (fairness)
    DISPLAY        : Co_Defs.Display_Type;  --  binding
    STACKSIZE      : Integer;               --  stack overflow if exceeded
    SUSPEND        : Integer;               --  id of object suspended on
    R1, R2, R3     : GRegister;             --  general use registers
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

  function GetClock return Time renames Clock;

  package File_Vectors is new Ada.Containers.Vectors (Positive, File_Ptr);

  type Task_Control_Blocks is array (TRange) of Task_Control_Block;

  --  Objects of type Interpreter_Data contains data that may be useful
  --  to be kept post-mortem or in a snapshot to "outside", or
  --  passed to the scheduler.

  type Interpreter_Data is record
    S           : Stack_Type;
    PS          : Processor_State;             --  Processor status register
    IR          : Order;                       --  Instruction register
    CurTask     : Integer;                     --  Index of currently executing task
    TCB         : Task_Control_Blocks;
    Files       : File_Vectors.Vector;
    Snap        : Boolean;   --  Snapshot flag to display scheduler status
    Nb_Callers  : Integer;   --  AVL  TERMINATE
    Nb_Complete : Integer;   --  AVL  TERMINATE
    EList       : Entry_Queue;
    TActive     : TRange;    --  no. of active tasks
    Start_Time  : Time;
    SWITCH      : Boolean;   --  invoke scheduler on next cycle flag
    SYSCLOCK    : Time;      --  (ms after 00:00:00 Jan 1, current year)
    TIMER       : Time;      --  set to end of current task's time slice
    Gen         : Ada.Numerics.Float_Random.Generator;
  end record;

  procedure Allocate_Text_File (
    ND : in out Interpreter_Data;
    R  : in out GRegister
  );

  procedure Free_Allocated_Contents (
    ND : in out Interpreter_Data
  );

  --  We have an "array of Character" (cf Is_Char_Array) on the stack
  function Get_String_from_Stack (ND : Interpreter_Data; Idx, Size : Defs.HAC_Integer) return String;

  procedure Pop (ND : in out Interpreter_Data; Amount : Positive := 1);
  procedure Push (ND : in out Interpreter_Data; Amount : Positive := 1);

  VM_Case_Check_Error            : exception;
  VM_Division_by_0               : exception;
  VM_End_Error                   : exception;
  VM_Function_End_without_Return : exception;
  VM_Out_of_Range                : exception;
  VM_Stack_Overflow              : exception;
  VM_Stack_Underflow             : exception;

  --  Post Mortem Dump of the task stack causing the exception
  --
  procedure Post_Mortem_Dump (CD: Compiler_Data; ND: In_Defs.Interpreter_Data);

end HAC.PCode.Interpreter.In_Defs;
