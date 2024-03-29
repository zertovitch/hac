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

with Ada.Finalization;

package HAC_Sys.Targets is

  type Machine is
    abstract new Ada.Finalization.Limited_Controlled with null record;

  type Abstract_Machine_Reference is access all Machine'Class;

  --------------------
  --  Informations  --
  --------------------

  function Name (m : Machine) return String is abstract;
  function CPU (m : Machine) return String is abstract;
  function OS (m : Machine) return String is abstract;
  function Null_Terminated_String_Literals (m : Machine) return Boolean is abstract;

  -------------------------------------------
  --  Initialize & Finalize Code Emission  --
  -------------------------------------------

  procedure Initialize_Code_Emission (m : in out Machine) is null;

  procedure Finalize_Code_Emission
    (m       : in out Machine;
     strings : in     String) is null;

  ----------------------------
  --  Machine Instructions  --
  ----------------------------

  procedure Emit_Arithmetic_Binary_Instruction
    (m         : in out Machine;
     operator  : in     Defs.Arithmetic_Binary_Operator;
     base_typ  : in     Defs.Numeric_Typ) is abstract;

  procedure Emit_Halt (m : in out Machine) is abstract;

  procedure Emit_Push_Discrete_Literal
    (m : in out Machine; x : Defs.HAC_Integer) is abstract;

  procedure Emit_Push_Discrete_Literals
    (m : in out Machine; x, y : Defs.HAC_Integer) is abstract;

  ----------------------------
  --  Built-In Subprograms  --
  ----------------------------

  procedure Emit_HAT_Builtin_Procedure
    (m            : in out Machine;
     builtin_proc : in     Defs.SP_Code;
     parameter    : in     Defs.HAC_Integer) is abstract;

  -------------
  --  Misc.  --
  -------------

  function Assembler_File_Name (m : Machine) return String is abstract;

  procedure Mark_Reference
    (m : in out Machine; located_id : Natural) is null;

  procedure Mark_Declaration
    (m : in out Machine; is_built_in : Boolean := False) is null;

  procedure Mark_Spec_Body_Cross_References
    (m                : in out Machine;
     spec_id, body_id : in     Positive) is null;

  combination_not_supported : exception;

end HAC_Sys.Targets;
