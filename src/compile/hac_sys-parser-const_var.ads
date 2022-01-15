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
--  Package around constants and variables declarations.

private package HAC_Sys.Parser.Const_Var is

  procedure Var_Declaration
    (CD         : in out Co_Defs.Compiler_Data;
     FSys       :        Defs.Symset;
     Block_Data : in out Block_Data_Type);

end HAC_Sys.Parser.Const_Var;
