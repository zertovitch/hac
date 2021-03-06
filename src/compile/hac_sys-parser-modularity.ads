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

with HAC_Sys.Li_Defs;

package HAC_Sys.Parser.Modularity is

  --  Context clause, 10.1.2.
  --
  --  Parse the bunch of "with" and "use" before a nesting level 0
  --  unit (which can be a subprogram or a package).
  --  Referenced units (specifications or body-only) are compiled if
  --  necessary, or reactivated as level 0 definitions.
  --
  procedure Context_Clause (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Li_Defs.Library_Data
  );

  --  Use clause. It is either part of a context clause, or a local declaration.
  --  8.4 (2)
  --
  procedure Use_Clause (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level
  );

end HAC_Sys.Parser.Modularity;
