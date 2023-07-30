-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

--  This package defines an interface for emitting machine
--  code in an abstract way. This allows to emit code for
--  various targets:
--
--    -  the HAC virtual machine ("p-code")
--    -  <CPU>_<OS>_<Assembler>
--
--  ! Goal: replace all "Emit" in the compiler with
--  ! method calls from this interface.
--  ! There will be likely hundreds of such methods in the end.
--  ! This will be done progressively. Be patient (or contribute)!

with HAC_Sys.Defs;

package HAC_Sys.Targets is

  type Abstract_Machine is interface;
  type Abstract_Machine_Reference is access Abstract_Machine'Class;

  function Name (m : Abstract_Machine) return String is abstract;
  function Is_HAC_VM (m : Abstract_Machine) return Boolean is abstract;

  procedure Initialize_Code_Emission (m : in out Abstract_Machine) is null;
  procedure Finalize_Code_Emission (m : in out Abstract_Machine) is null;

  procedure Emit_HAT_Builtin_Procedure
    (m            : in out Abstract_Machine;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer) is abstract;

end HAC_Sys.Targets;
