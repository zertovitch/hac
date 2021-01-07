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

package HAC_Sys.Parser.Modularity is

  --  Context clause: the bunch of "with" and "use" before a unit (a main program or a package)
  --  10.1.2
  --
  procedure Context_Clause (CD : in out Compiler_Data);

  --  Use clause. It is either part of a context clause, or a local declaration.
  --  8.4 (2)
  --
  procedure Use_Clause (CD : in out Compiler_Data; Level : Defs.Nesting_level);

end HAC_Sys.Parser.Modularity;
