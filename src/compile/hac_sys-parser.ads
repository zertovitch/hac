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

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs;

package HAC_Sys.Parser is

  type Block_Data_Type is record
    level                           : Defs.Nesting_level;
    is_a_function                   : Boolean;             --  RETURN [Value] statement expected
    block_id_index                  : Integer;
    initialization_object_code_size : Integer;             --  Was: ICode
    data_allocation_index           : Integer;             --  Was: DX
    max_data_allocation_index       : Integer;             --  Was: MaxDX
    --  ^ includes the maximum space needed
    --    for parameters of FOR loops.
  end record;

  procedure Block (
    CD                   : in out Co_Defs.Compiler_Data;
    FSys                 :        Defs.Symset;
    Is_a_block_statement :        Boolean;        --  RM: 5.6 Block Statements
    Initial_Block_Data   :        Block_Data_Type;
    Block_Id             :        Defs.Alfa;      --  Name of this block (if any)
    Block_Id_with_case   :        Defs.Alfa
  );

  --  E.g. : in the case of a block statement within a function, the value
  --  True will be passed for both Is_a_function and Is_a_block_statement.
  --  When Is_a_block_statement = True, the current symbol Sy must be either
  --  DECLARE_symbol or BEGIN_symbol

end HAC_Sys.Parser;
