-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

--  The target Semantics produces no machine code but
--  serves semantics analysis purposes, like cross-references
--  for an editor with auto-complete, contextual menus and tool tips.

with HAC_Sys.Co_Defs;

package HAC_Sys.Targets.Semantics is

  --  Declarations map
  --  ================
  --  
  --  Key                  Value
  --  ===                  =====
  --  index in Id Table    file_name, i, j of declaration
  --  
  --  
  --  Reference map
  --  =============
  --  
  --  Key                  Value                  Note
  --  ===                  =====                  ====
  --  [file_name i j]      index in Id Table      compare identifiers first
  --  

  type Machine is limited new Targets.Machine with record
    CD : Co_Defs.Compiler_Data_Access;
    --    ^ We keep this field public for convenience (it is
    --      needed in method Set_Target in Co_Defs).
  end record;

  --------------------
  --  Informations  --
  --------------------

  overriding function Name (m : Machine) return String is ("HAC Semantics");
  overriding function CPU (m : Machine) return String is ("No CPU");
  overriding function OS (m : Machine) return String is ("Any");
  overriding function Null_Terminated_String_Literals (m : Machine) return Boolean is (False);

  ----------------------------
  --  Machine Instructions  --
  ----------------------------

  overriding procedure Emit_Arithmetic_Binary_Instruction
    (m         : in out Machine;
     operator  :        Defs.Arithmetic_Binary_Operator;
     base_typ  :        Defs.Numeric_Typ) is null;

  overriding procedure Emit_Halt (m : in out Machine) is null;

  overriding procedure Emit_Push_Discrete_Literal
    (m : in out Machine; x : Defs.HAC_Integer) is null;

  overriding procedure Emit_Push_Discrete_Literals
    (m : in out Machine; x, y : Defs.HAC_Integer) is null;

  ----------------------------
  --  Built-In Subprograms  --
  ----------------------------

  overriding procedure Emit_HAT_Builtin_Procedure
    (m            : in out Machine;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer) is null;

  -------------
  --  Misc.  --
  -------------

  overriding function Assembler_File_Name (m : Machine) return String is ("");

  overriding procedure Mark_Declaration (m : Machine);

  overriding procedure Mark_Reference (m : Machine);

end HAC_Sys.Targets.Semantics;
