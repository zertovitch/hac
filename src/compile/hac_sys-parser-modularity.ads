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

with HAC_Sys.Librarian;

package HAC_Sys.Parser.Modularity is

  --  Context clause, 10.1.2.
  --
  --  Parse the bunch of "with" and "use" before a library level
  --  unit (which can be a subprogram or a package).
  --  Referenced units (specifications or body-only) are compiled if
  --  necessary, or reactivated as library level definitions.
  --
  procedure Context_Clause (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Librarian.Library_Data
  );

end HAC_Sys.Parser.Modularity;
