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

with HAC_Sys.Co_Defs, HAC_Sys.Defs, HAC_Sys.PCode;

package HAC_Sys.Parser is

  use Co_Defs;

  procedure Block (
    CD                   : in out Compiler_Data;
    FSys                 :        Defs.Symset;
    Is_a_function        :        Boolean;        --  RETURN [Value] statement expected
    Is_a_block_statement :        Boolean;        --  RM: 5.6 Block Statements
    Initial_Level        :        PCode.Nesting_level;
    Prt                  :        Integer;
    Block_ID             :        Defs.Alfa;  --  Name of this block (if any)
    Block_ID_with_case   :        Defs.Alfa
  );

  --  E.g. : in the case of a block statement within a function, the value
  --  True will be passed for both Is_a_function and Is_a_block_statement.
  --  When Is_a_block_statement = True, the current symbol Sy must be either
  --  DECLARE_symbol or BEGIN_symbol

end HAC_Sys.Parser;
