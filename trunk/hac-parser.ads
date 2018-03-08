-------------------------------------------------------------------------------------
--
-- HAC - HAC Ada Compiler
--
-- A compiler in Ada for an Ada subset
--
-- Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC.Data;

package HAC.Parser is

  procedure Block(
    FSys                 : HAC.Data.Symset;
    Is_a_function        : Boolean;       --  RETURN [Value] statement expected
    Is_a_block_statement : Boolean;       --  RM: 5.6 Block Statements
    Level_A              : Integer;
    Prt                  : Integer;
    BlockID              : HAC.Data.Alfa  --  Name of this block (if any)
  );

  -- E.g. : in the case of a block statement within a function, the value
  -- True will be passed for both Is_a_function and Is_a_block_statement.
  -- When Is_a_block_statement = True, the current symbol Sy must be either
  -- DECLARE_symbol or BEGIN_symbol

end HAC.Parser;
