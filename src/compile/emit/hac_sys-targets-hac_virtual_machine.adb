package body HAC_Sys.Targets.HAC_Virtual_Machine is

  procedure Emit_HAT_Builtin_Procedure
    (m            : in out HAC_VM;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer)
  is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Emit_HAT_Console_Put unimplemented");
      raise Program_Error with "Unimplemented procedure Emit_HAT_Console_Put";
   end Emit_HAT_Builtin_Procedure;

end HAC_Sys.Targets.HAC_Virtual_Machine;
