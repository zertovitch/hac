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

with HAC.Data, HAC.PCode;

package HAC.Compiler is

  -----------------------
  --  Compiler tables  --
  -----------------------
  --
  --  WIP from 2020-04-19: we progressively stuff everything into compiler & scanner objects
  --  until there is no more global data in HAC.Data.

  subtype Fixed_Size_Object_Code_Table is HAC.PCode.Object_Code_Table (0 .. HAC.Data.CDMax);

  type Compiler_Tables is record
    ObjCode : Fixed_Size_Object_Code_Table;
    LC      : Integer;                   --  location counter in the Object_Code_Table
  end record;

  --  Main compilation procedure.
  --
  procedure Compile (
    CT                 : in out Compiler_Tables;
    asm_dump_file_name : String := ""
  );

  procedure Emit (
    CT   : in out Compiler_Tables;
    FCT  :        HAC.PCode.Opcode);

  procedure Emit1 (
    CT   : in out Compiler_Tables;
    FCT  :        HAC.PCode.Opcode;
    B    :        Integer);

  procedure Emit2 (
    CT   : in out Compiler_Tables;
    FCT  :        HAC.PCode.Opcode;
    a, B :        Integer);

  procedure Emit_Comparison_Instruction (
    CT        : in out HAC.Compiler.Compiler_Tables;
    Operator  :        HAC.Data.Comparison_Operator;
    Base_Type :        HAC.Data.Types
  );

  procedure Emit_Unary_Minus (
    CT        : in out HAC.Compiler.Compiler_Tables;
    Base_Type :        HAC.Data.Numeric_Typ
  );

  procedure Emit_Arithmetic_Binary_Instruction (
    CT        : in out HAC.Compiler.Compiler_Tables;
    Operator  :        HAC.Data.Arithmetic_Binary_Operator;
    Base_Type :        HAC.Data.Numeric_Typ
  );

end HAC.Compiler;
