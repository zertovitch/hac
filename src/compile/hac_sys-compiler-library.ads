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

with HAC_Sys.PCode;

package HAC_Sys.Compiler.Library is

  Library_Level : constant := 0;

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

  procedure Apply_USE_Clause (
    CD       : in out Compiler_Data;
    Level    : in     HAC_Sys.PCode.Nesting_level;
    Pkg_Name : in     String
  );

  procedure Apply_WITH_Standard (CD : in out Compiler_Data);

  procedure Apply_WITH_HAC_Pack (CD : in out Compiler_Data);

end HAC_Sys.Compiler.Library;
