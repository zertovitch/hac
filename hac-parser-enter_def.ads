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

with HAC.Compiler, HAC.Data, HAC.PCode;

package HAC.Parser.Enter_Def is  --  Package around entering definitions.

  procedure Enter_Block (
    CD    : in out Compiler.Compiler_Data;
    Tptr  :        Integer
  );

  procedure Enter (
    CD               : in out Compiler.Compiler_Data;
    Level            :        PCode.Nesting_level;
    Id, Id_with_case :        Data.Alfa;
    K                :        Compiler.aObject
  );

end HAC.Parser.Enter_Def;
