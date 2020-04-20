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

with HAC.Compiler;

package HAC.Scanner is

  use HAC.Compiler;

  --  Source code scanning for the compiler

  procedure InSymbol (CD : in out Compiler_Data);
  --  Communicates via global variables in HAC.Data (arrgh!)
  --  WIP: moving globals to CD.

end HAC.Scanner;
