-------------------------------------------------------------------------------------
--
-- HAC - HAC Ada Compiler
--
-- A compiler in Ada for an Ada subset

-- Legal licensing note:

--  Copyright (c) 2013 Gautier de Montmollin
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

package HAC.UErrors is

  procedure Error(N: Integer);
  --
  undefined_identifier         : constant:=  0;
  duplicate_identifier         : constant:=  1;
  identifier_missing           : constant:=  2;
  closing_parenthesis_missing  : constant:=  4;
  colon_missing                : constant:=  5;
  semicolon_missing            : constant:= 14;
  IS_missing                   : constant:= 20;
  END_missing                  : constant:= 57;

  procedure EndSkip;

  procedure ErrorMsg;

  procedure Fatal(N: Integer); -- internal table overflow
  --
  IDENTIFIERS_table_overflow    : constant:=  1;
  FLOAT_constants_table_overflow: constant:=  3;
  LEVEL_overflow                : constant:=  5;
  OBJECT_overflow               : constant:=  6;
  PATCHING_overflow             : constant:= 10;

  Failure_1_0: exception;

  function ErrorString(Id: Integer) return String; -- for debug

end HAC.UErrors;
