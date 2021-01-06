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

package HAC_Sys.Compiler.Library is

  ----------------------
  --  Built-in units  --
  ----------------------

  procedure Enter_Built_In (
    CD             : in out Compiler_Data;
    Full_Ident     : in     String;  --  "Main", "Standard.False", ...
    New_Entity     : in     Entity_Kind;
    Base_Type      : in     Typen;
    Size           : in     Integer;
    Discrete_First : in     HAC_Integer := 0;
    Discrete_Last  : in     HAC_Integer := 0
  );

  --  NB: later we will split Enter_* into a With_* and a Use_*.

  procedure Enter_Standard (CD : in out Compiler_Data);

  procedure Enter_HAC_Pack (CD : in out Compiler_Data);

end HAC_Sys.Compiler.Library;
