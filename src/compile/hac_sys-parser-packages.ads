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
--  Parse packages right after the "PACKAGE [BODY] name" symbols.
--  For package declaration: latest identifier is entered as "Paquetage" kind.

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs;

package HAC_Sys.Parser.Packages is

  procedure Package_Declaration (
    CD                   : in out Co_Defs.Compiler_Data;
    FSys                 :        Defs.Symset
  );

  procedure Package_Body (
    CD                   : in out Co_Defs.Compiler_Data;
    FSys                 :        Defs.Symset
  );

end HAC_Sys.Parser.Packages;
