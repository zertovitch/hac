-------------------------------------------------------------------------------------
--
-- HAC - HAC Ada Compiler
--
-- A compiler in Ada for an Ada subset

-- Legal licensing note:

--  Copyright (c) 2013..2014 Gautier de Montmollin
--
--  History and authors list of works HAC was originally derived from
--  can be found in hac.txt.

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 12-Sep-2013 on the site
-- http://www.opensource.org/licenses/mit-license.php

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
