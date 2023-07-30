-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------

--  Here is the HAC Virtual Machine ("p-code") code emission

with HAC_Sys.Co_Defs;

package HAC_Sys.Targets.HAC_Virtual_Machine is

  type HAC_VM is new Abstract_Machine with record
    CD : Co_Defs.Compiler_Data_Access;
    --  ^ In the future the instruction table and other items
    --    will be stored here and we can remove CD.
  end record;

  procedure Emit_HAT_Builtin_Procedure
    (m            : in out HAC_VM;
     builtin_proc :        Defs.SP_Code;
     parameter    :        Defs.HAC_Integer);

end HAC_Sys.Targets.HAC_Virtual_Machine;
