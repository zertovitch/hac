-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

--  Here is the code emission for the original HAC target, the
--  HAC Virtual Machine ("p-code").

with HAC_Sys.Co_Defs;

package HAC_Sys.Targets.HAC_Virtual_Machine is

  type Machine is limited new Targets.Machine with record
    CD : Co_Defs.Compiler_Data_Access;
    --  ^ In the future, the instruction table and other items
    --    will be stored here and we can remove CD.
    --    As long as this field exists, we keep it public for
    --    convenience (it is needed in method Set_Target in Co_Defs).
  end record;

  --------------------
  --  Informations  --
  --------------------

  overriding function Name (m : Machine) return String is ("HAC Virtual Machine");
  overriding function CPU (m : Machine) return String is ("HAC VM");
  overriding function OS (m : Machine) return String is ("Any");
  overriding function Null_Terminated_String_Literals (m : Machine) return Boolean is (False);

  ----------------------------
  --  Machine Instructions  --
  ----------------------------

  overriding procedure Emit_Arithmetic_Binary_Instruction
    (m         : in out Machine;
     operator  :        Defs.Arithmetic_Binary_Operator;
     base_typ  :        Defs.Numeric_Typ);

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

  -------------
  --  Misc.  --
  -------------

  overriding function Assembler_File_Name (m : Machine) return String is ("asm_dump.pca");
  --  PCA = PCode Assembler

end HAC_Sys.Targets.HAC_Virtual_Machine;
