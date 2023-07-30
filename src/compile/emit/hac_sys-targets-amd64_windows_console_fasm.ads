-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

with HAT;

package HAC_Sys.Targets.AMD64_Windows_Console_FASM is

  type Machine is limited new Abstract_Machine with record
    asm_file : HAT.File_Type;
  end record;

  --------------------
  --  Informations  --
  --------------------

  overriding function Name (m : Machine) return String is ("Windows 64 Console");
  overriding function Is_HAC_VM (m : Machine) return Boolean is (False);
  overriding function CPU (m : Machine) return String is ("AMD64");
  overriding function OS (m : Machine) return String is ("Windows");
  overriding function Null_Terminated_String_Literals (m : Machine) return Boolean is (True);

  -------------------------------------------
  --  Initialize & Finalize Code Emission  --
  -------------------------------------------

  overriding procedure Initialize_Code_Emission (m : in out Machine);
  overriding procedure Finalize_Code_Emission
    (m       : in out Machine;
     strings :        String);

  ----------------------------
  --  Machine Instructions  --
  ----------------------------

  overriding procedure Emit_Halt (m : in out Machine);

  overriding procedure Emit_Push_Discrete_Literal
    (m : in out Machine; x : Defs.HAC_Integer);

  overriding procedure Emit_Push_Discrete_Literals
    (m : in out Machine; x, y : Defs.HAC_Integer);

  ----------------------------
  --  Built-In Subprograms  --
  ----------------------------

  overriding procedure Emit_HAT_Builtin_Procedure
    (m            : in out Machine;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer);

end HAC_Sys.Targets.AMD64_Windows_Console_FASM;
