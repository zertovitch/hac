with HAC.Co_Defs;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;
with Ada.Unchecked_Deallocation;

package HAC.PCode.Interpreter.In_Defs is

  NilTask : constant := -1;

  subtype TRange is Integer range 0 .. Defs.TaskMax;  --  task index

  type Processor_State is (
    RUN,               --  RUN is the normal processor state
    --
    Case_Check_Error,  --  Happens when a case was not found in a CASE statement.
    DEADLOCK,
    DIVCHK,            --  Division by 0         !! -> Exception_Raised with Contraint_Error
    FIN,
    INXCHK,            --  Out-of-range error    !! -> Exception_Raised with Contraint_Error
    ProgErr,           --  Program_Error         !! -> Exception_Raised with Program_Error
    REDCHK,            --  End_Error             !! -> Exception_Raised with End_Error
    STKCHK,            --  Stack overflow        !! -> Exception_Raised with (Storage_Error, "Stack overflow")
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
    DISPLAY : Co_Defs.Display_Type;
    --  stack overflow if exceeded
    STACKSIZE : Integer;
    --  id of object suspended on
    SUSPEND : Integer;
    --  general use registers
    R1, R2, R3 : GRegister;
  end record;

  type Enode;
  type Eptr is access Enode; --  task entry rendzv pointer

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
    PS          : Processor_State := RUN;  --  Processor status register
    IR          : Order;                   --  Instruction register
    CurTask     : Integer;                 --  Index of currently executing task
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

  procedure Pop (ND : in out Interpreter_Data; Amount : Positive := 1);

  procedure Push (ND : in out Interpreter_Data; Amount : Positive := 1);

  Stack_Overflow, Stack_Underflow : exception;

end HAC.PCode.Interpreter.In_Defs;
