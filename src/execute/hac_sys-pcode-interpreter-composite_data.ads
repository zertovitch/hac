with HAC_Sys.PCode.Interpreter.In_Defs;

package HAC_Sys.PCode.Interpreter.Composite_Data is
  use In_Defs;

  -----------------------
  --  VM Instructions  --
  -----------------------

  --  Execute instruction stored as Opcode in ND.IR.F.
  --  ND.IR.F is in the Composite_Data_Opcode subtype range.
  procedure Do_Composite_Data_Operation (CD : Compiler_Data; ND : in out Interpreter_Data);

end HAC_Sys.PCode.Interpreter.Composite_Data;
