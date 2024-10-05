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
     context           :        Defs.Flow_Context;
     var_id_index      :        Integer;
     check_is_variable :        Boolean);

  procedure Sequence_of_Statements  --  Ada RM 5.1 (2)
    (CD         : in out Co_Defs.Compiler_Data;
     sentinel   :        Defs.Symset;
     block_data : in out Block_Data_Type;
     optional   :        Boolean := False);

  procedure Sequence_of_Statements_in_a_Conditional_Statement
    (CD         : in out Co_Defs.Compiler_Data;
     sentinel   :        Defs.Symset;
     block_data : in out Block_Data_Type;
     optional   :        Boolean := False);

end HAC_Sys.Parser.Statements;
