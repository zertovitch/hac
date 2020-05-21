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

with HAC.Compiler, HAC.Defs, HAC.PCode;

private package HAC.Parser.Enter_Def is  --  Package around entering definitions.

  procedure Enter_Block (
    CD    : in out Compiler.Compiler_Data;
    Tptr  :        Integer
  );

  procedure Enter (
    CD               : in out Compiler.Compiler_Data;
    Level            :        PCode.Nesting_level;
    Id, Id_with_case :        Defs.Alfa;
    K                :        Compiler.aObject
  );

  procedure Enter_Array (
    CD       : in out Compiler.Compiler_Data;
    Index_TP :        Compiler.Exact_Typ;
    L, H     : Integer
  );

  procedure Enter_Variables (
    CD    : in out Compiler.Compiler_Data;
    Level :        PCode.Nesting_level
  );

end HAC.Parser.Enter_Def;
