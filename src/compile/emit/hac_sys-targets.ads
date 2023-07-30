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

  type Abstract_Machine is limited interface;
  type Abstract_Machine_Reference is access Abstract_Machine'Class;

  --------------------
  --  Informations  --
  --------------------

  function Name (m : Abstract_Machine) return String is abstract;
  function Is_HAC_VM (m : Abstract_Machine) return Boolean is abstract;
  function CPU (m : Abstract_Machine) return String is abstract;
  function OS (m : Abstract_Machine) return String is abstract;
  function Null_Terminated_String_Literals (m : Abstract_Machine) return Boolean is abstract;

  -------------------------------------------
  --  Initialize & Finalize Code Emission  --
  -------------------------------------------

  procedure Initialize_Code_Emission (m : in out Abstract_Machine) is null;

  procedure Finalize_Code_Emission
    (m       : in out Abstract_Machine;
     strings :        String) is null;

  ----------------------------
  --  Machine Instructions  --
  ----------------------------

  procedure Emit_Halt (m : in out Abstract_Machine) is abstract;

  procedure Emit_Push_Discrete_Literal
    (m : in out Abstract_Machine; x : Defs.HAC_Integer) is abstract;

  procedure Emit_Push_Discrete_Literals
    (m : in out Abstract_Machine; x, y : Defs.HAC_Integer) is abstract;

  ----------------------------
  --  Built-In Subprograms  --
  ----------------------------

  procedure Emit_HAT_Builtin_Procedure
    (m            : in out Abstract_Machine;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer) is abstract;

  combination_not_supported : exception;

end HAC_Sys.Targets;
