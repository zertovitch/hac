with HAC_Sys.Co_Defs,
     HAC_Sys.PCode.Interpreter.In_Defs;

private package HAC_Sys.PCode.Interpreter.Tasking is
  use Co_Defs, In_Defs;

  -----------------------
  --  VM Instructions  --
  -----------------------

  --  Execute tasking instruction stored as Opcode in ND.IR.F.
  --  ND.IR.F is in the Tasking_Opcode subtype range.
  procedure Do_Tasking_Operation (CD : Compiler_Data; ND : in out Interpreter_Data);

  -------------
  --  Misc.  --
  -------------

  function Any_Task_Delayed (CD : Compiler_Data; ND : Interpreter_Data) return Boolean;

  function EIndex (CD : Compiler_Data; Entry_Index : Integer) return Integer;

  procedure Init_main_task (CD : Compiler_Data; ND : in out Interpreter_Data);
  procedure Init_other_tasks (CD : Compiler_Data; ND : in out Interpreter_Data);

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

  procedure Wake_Tasks (
    CD     :        Compiler_Data;
    ND     : in out Interpreter_Data;
    Result :    out Boolean
  );

  --  Default Task time-slice in seconds
  --  Feldman: 60ths of a sec on Mac
  TSlice : constant Duration := 0.016666666;

  procedure Manage_Scheduling (CD : Compiler_Data; ND : in out Interpreter_Data);

end HAC_Sys.PCode.Interpreter.Tasking;
