with HAC_Sys.PCode.Interpreter.In_Defs;

package HAC_Sys.PCode.Interpreter.Operators is
  use In_Defs;

  -----------------------
  --  VM Instructions  --
  -----------------------

  --  Execute operator stored as Opcode in ND.IR.F.
  procedure Do_Unary_Operator (ND : in out Interpreter_Data);
  procedure Do_Binary_Operator (ND : in out Interpreter_Data);

  --  Execute operator through the Opcode k_Standard_Functions.
  procedure Do_SF_Operator (CD : Compiler_Data; ND : in out Interpreter_Data);

end HAC_Sys.PCode.Interpreter.Operators;
