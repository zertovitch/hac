with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.PCode;

package body HAC_Sys.Targets.HAC_Virtual_Machine is

  use Compiler.PCode_Emit, PCode;

  overriding procedure Emit_HAT_Builtin_Procedure
    (m            : in out HAC_VM;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer)
  is
   begin
     Emit_2 (m.CD.all, k_HAT_Procedure, Defs.SP_Code'Pos (builtin_proc), parameter);
   end Emit_HAT_Builtin_Procedure;

end HAC_Sys.Targets.HAC_Virtual_Machine;
