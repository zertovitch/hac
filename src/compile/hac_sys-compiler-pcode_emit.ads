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

package HAC_Sys.Compiler.PCode_Emit is

  use PCode;

  procedure Emit (
    CD   : in out Compiler_Data;
    FCT  :        Opcode
  );

  procedure Emit_1 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    B    :        Operand_2_Type
  );

  procedure Emit_2 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    a    :        Operand_1_Type;
    B    :        Operand_2_Type
  );

  procedure Emit_Std_Funct (
    CD    : in out Compiler_Data;
    Code  :        SF_Code;
    Extra :        Operand_1_Type := 0
  );

  procedure Emit_Comparison_Instruction (
    CD        : in out Compiler_Data;
    Operator  :        Comparison_Operator;
    Base_Typ  :        Typen
  );

  procedure Emit_Unary_Minus (
    CD        : in out Compiler_Data;
    Base_Typ  :        Numeric_Typ
  );

  procedure Emit_Arithmetic_Binary_Instruction (
    CD        : in out Compiler_Data;
    Operator  :        Arithmetic_Binary_Operator;
    Base_Typ  :        Numeric_Typ
  );

end HAC_Sys.Compiler.PCode_Emit;
