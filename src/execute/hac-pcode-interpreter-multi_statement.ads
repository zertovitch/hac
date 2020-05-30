with HAC.PCode.Interpreter.In_Defs;

package HAC.PCode.Interpreter.Multi_Statement is
  use In_Defs;

  -----------------------
  --  VM Instructions  --
  -----------------------

  --  Execute instruction stored as Opcode in ND.IR.F.
  --  ND.IR.F is in the Multi_Statement_Opcode subtype range.
  procedure Do_Multi_Statement_Operation (CD : Compiler_Data; ND : in out Interpreter_Data);

end HAC.PCode.Interpreter.Multi_Statement;
