with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.PCode;

package body HAC_Sys.Targets.HAC_Virtual_Machine is

  use Compiler.PCode_Emit, PCode;

  overriding procedure Emit_Halt (m : in out Machine) is
  begin
    Emit (m.CD.all, k_Halt_Interpreter);
  end Emit_Halt;

  overriding procedure Emit_Push_Discrete_Literal
    (m : in out Machine; x : Defs.HAC_Integer)
  is
  begin
    Emit_1 (m.CD.all, k_Push_Discrete_Literal, x);
  end Emit_Push_Discrete_Literal;

  overriding procedure Emit_Push_Discrete_Literals
    (m : in out Machine; x, y : Defs.HAC_Integer)
  is
  begin
    Emit_2 (m.CD.all, k_Push_Two_Discrete_Literals, x, y);
  end Emit_Push_Discrete_Literals;

  overriding procedure Emit_HAT_Builtin_Procedure
    (m            : in out Machine;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer)
  is
   begin
     Emit_2 (m.CD.all, k_HAT_Procedure, Defs.SP_Code'Pos (builtin_proc), parameter);
   end Emit_HAT_Builtin_Procedure;

end HAC_Sys.Targets.HAC_Virtual_Machine;
