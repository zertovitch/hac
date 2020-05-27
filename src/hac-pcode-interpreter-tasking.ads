with HAC.PCode.Interpreter.In_Defs;

package HAC.PCode.Interpreter.Tasking is
  use In_Defs;

  procedure Do_Accept_Rendezvous (CD : Compiler_Data; ND: in out Interpreter_Data);

  procedure Do_Selective_Wait (CD : Compiler_Data; ND: in out Interpreter_Data);

end HAC.PCode.Interpreter.Tasking;
