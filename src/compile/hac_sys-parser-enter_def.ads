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

with HAC_Sys.Defs;

package HAC_Sys.Parser.Enter_Def is  --  Package around entering definitions.

  use Defs;

  procedure Enter_Block (
    CD    : in out Compiler_Data;
    Tptr  :        Integer
  );

  procedure Enter (
    CD               : in out Compiler_Data;
    Level            :        Defs.Nesting_level;
    Id, Id_with_case :        Defs.Alfa;
    K                :        Entity_Kind
  );

  procedure Enter_Array (
    CD       : in out Compiler_Data;
    Index_TP :        Exact_Typ;
    L, H     :        Integer
  );

  procedure Enter_Variables (
    CD    : in out Compiler_Data;
    Level :        Defs.Nesting_level
  );

end HAC_Sys.Parser.Enter_Def;
