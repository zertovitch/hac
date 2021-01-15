with HAC_Sys.Co_Defs,
     HAC_Sys.PCode.Interpreter.In_Defs;

private package HAC_Sys.PCode.Interpreter.Calls is

  -----------------------
  --  VM Instructions  --
  -----------------------

  --  Execute instruction stored as Opcode in ND.IR.F.
  --  ND.IR.F is in the Calling_Opcode subtype range.
  procedure Do_Calling_Operation (
    CD :        Co_Defs.Compiler_Data;
    ND : in out In_Defs.Interpreter_Data
  );

end HAC_Sys.PCode.Interpreter.Calls;
