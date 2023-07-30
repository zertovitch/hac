-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

package HAC_Sys.Targets.HAC_Virtual_Machine is

  type HAC_VM is new Abstract_Machine with record
    dummy : Integer;
  end record;

  procedure Emit_HAT_Builtin_Procedure
    (m            : in out HAC_VM;
     builtin_proc :        Integer;
     parameter    :        Integer);

end HAC_Sys.Targets.HAC_Virtual_Machine;
