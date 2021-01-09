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

with HAC_Sys.Co_Defs;

package HAC_Sys.Scanner is

  --  Source code scanning for the compiler

  procedure InSymbol (CD : in out Co_Defs.Compiler_Data);

  procedure Skip_Blanks (CD : in out Co_Defs.Compiler_Data);

end HAC_Sys.Scanner;
