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

private package HAC_Sys.Parser.Statements is

  procedure Assignment
    (CD                : in out Co_Defs.Compiler_Data;
     FSys              :        Defs.Symset;
     Level             :        Defs.Nesting_Level;
     Var_Id_Index      :        Integer;
     check_is_variable :        Boolean);

  procedure Sequence_of_Statements  --  Ada RM 5.1 (2)
    (CD         : in out Co_Defs.Compiler_Data;
     Sentinel   :        Defs.Symset;
     Block_Data : in out Block_Data_Type;
     Optional   :        Boolean := False);

end HAC_Sys.Parser.Statements;
