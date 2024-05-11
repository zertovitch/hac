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
--  This package facilitates the code emission for the HAC VM, but will
--  shrink in the future in favour of HAC_Sys.Targets.HAC_Virtual_Machine.

with HAC_Sys.Defs,
     HAC_Sys.PCode;

package HAC_Sys.Compiler.PCode_Emit is

  --  This layer could be developed to abstract the virtual machine
  --  code emission, for instance for producing real machine code.

  use Defs, PCode;

  procedure Emit
    (CD   : in out Compiler_Data;
     FCT  :        Opcode);

  procedure Emit_1
    (CD   : in out Compiler_Data;
     FCT  :        Opcode;
     B    :        Operand_2_Type);

  procedure Emit_2
    (CD   : in out Compiler_Data;
     FCT  :        Opcode;
     a    :        Operand_1_Type;
     B    :        Operand_2_Type);

  procedure Emit_3
    (CD   : in out Compiler_Data;
     FCT  :        Opcode;
     a    :        Operand_1_Type;
     B    :        Operand_2_Type;
     c    :        Operand_3_Type);

  procedure Emit_Std_Funct
    (CD    : in out Compiler_Data;
     Code  :        SF_Code;
     Extra :        Operand_1_Type := 0);

  procedure Emit_Comparison_Instruction
    (CD        : in out Compiler_Data;
     Operator  :        Comparison_Operator;
     Base_Typ  :        Typen);

  procedure Emit_Unary_Minus
    (CD        : in out Compiler_Data;
     Base_Typ  :        Numeric_Typ);

  procedure Emit_Push_Float_Literal
    (CD : in out Compiler_Data;
     X  :        HAC_Float);

  procedure Enter_or_find_Float
    (CD         : in out Compiler_Data;
     X          :        HAC_Float;
     RNum_Index :    out Natural);

  procedure Emit_Lower_Bound_Check
    (CD : in out Compiler_Data; S : Exact_Subtyp);

  procedure Emit_Upper_Bound_Check
    (CD : in out Compiler_Data; S : Exact_Subtyp);

end HAC_Sys.Compiler.PCode_Emit;
