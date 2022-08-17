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
--  This package provides the insertion "by hand" of packages
--  of HAC's run-time library into compiler and library data.

with HAC_Sys.Co_Defs;

private package HAC_Sys.Librarian.Built_In_Packages is

  ---------------------------------------------
  --  Enter "manually" the Standard package  --
  ---------------------------------------------

  procedure Define_and_Register_Standard (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Library_Data
  );

  -----------------------------------------------
  --  Enter "manually" the Interfaces package  --
  -----------------------------------------------

  procedure Define_and_Register_Interfaces (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Library_Data
  );

  ----------------------------------------
  --  Enter "manually" the HAT package  --
  ----------------------------------------

  procedure Define_and_Register_HAT (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Library_Data
  );

end HAC_Sys.Librarian.Built_In_Packages;
