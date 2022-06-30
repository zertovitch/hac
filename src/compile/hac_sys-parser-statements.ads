-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

package HAC_Sys.Parser.Statements is

  procedure Assignment
    (CD              : in out Co_Defs.Compiler_Data;
     FSys            :        Defs.Symset;
     Level           :        Defs.Nesting_level;
     Var_Id_Index    :        Integer;
     Check_read_only :        Boolean);

  procedure Sequence_of_Statements  --  Ada RM 5.1 (2)
    (CD         : in out Co_Defs.Compiler_Data;
     Sentinel   :        Defs.Symset;
     Block_Data : in out Block_Data_Type;
     Optional   :        Boolean := False);

end HAC_Sys.Parser.Statements;
