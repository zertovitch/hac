with HAC.PCode.Interpreter.In_Defs;

package HAC.PCode.Interpreter.Tasking is
  use In_Defs;

  -----------------------
  --  VM Instructions  --
  -----------------------

  procedure Do_Accept_Rendezvous (CD : Compiler_Data; ND: in out Interpreter_Data);
  procedure Do_End_Rendezvous (CD : Compiler_Data; ND: in out Interpreter_Data);

  procedure Do_Selective_Wait (CD : Compiler_Data; ND: in out Interpreter_Data);

  procedure Do_Signal_Semaphore (CD : Compiler_Data; ND: in out Interpreter_Data);
  procedure Do_Wait_Semaphore (ND: in out Interpreter_Data);

  -------------
  --  Misc.  --
  -------------

  function Any_Task_Delayed (CD : Compiler_Data; ND: Interpreter_Data) return Boolean;

  function EIndex (CD : Compiler_Data; Entry_Index : Integer) return Integer;

  procedure Init_main_task (CD : Compiler_Data; ND: in out Interpreter_Data);
  procedure Init_other_tasks (CD : Compiler_Data; ND: in out Interpreter_Data);

  procedure Queue (
    CD           :        Compiler_Data;
    ND           : in out Interpreter_Data;
    Entry_Index  :        Integer;
    Calling_Task :        TRange
  );

  procedure ShowQ (
    CD          : Compiler_Data;
    ND          : in out Interpreter_Data;
    Entry_Index : Integer
  );

  procedure Tasks_to_wake (
    CD     :        Compiler_Data;
    ND     : in out Interpreter_Data;
    Result :    out Boolean
  );

  --  Default Task time-slice in seconds
  --  Feldman: 60ths of a sec on Mac
  TSlice : constant Duration := 0.016666666;

end HAC.PCode.Interpreter.Tasking;
