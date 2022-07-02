with HAC_Sys.Builder,
     HAC_Sys.PCode.Interpreter.In_Defs;

private package HAC_Sys.PCode.Interpreter.Operators is

  -----------------------
  --  VM Instructions  --
  -----------------------

  --  Execute operator stored as Opcode in ND.IR.F.
  procedure Do_Unary_Operator (ND : in out In_Defs.Interpreter_Data);
  procedure Do_Binary_Operator (ND : in out In_Defs.Interpreter_Data);
  procedure Do_Multiple_Operator (ND : in out In_Defs.Interpreter_Data);
  procedure Do_Special_Operator (ND : in out In_Defs.Interpreter_Data);

  --  Execute operator through the Opcode k_Standard_Functions.
  procedure Do_SF_Operator (BD : Builder.Build_Data; ND : in out In_Defs.Interpreter_Data);

end HAC_Sys.PCode.Interpreter.Operators;
