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

with HAC.UErrors;

package body HAC.Compiler.PCode_Emit is

  function Compiler_Data_to_Debug_Info (CD : Compiler_Data) return Debug_Info is
  begin
    return (Line_Number   => CD.Line_Count,
            Full_Block_Id => CD.Full_Block_Id,
            File_Name     => CD.source_file_name);
  end Compiler_Data_to_Debug_Info;

  procedure Emit (
    CD   : in out Compiler_Data;
    FCT  :        Opcode
  )
  is
  begin
    PCode.Emit (CD.ObjCode, CD.LC, Compiler_Data_to_Debug_Info (CD), FCT);
  end Emit;

  procedure Emit1 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    B    :        Operand_2_Type
  )
  is
  begin
    PCode.Emit1 (CD.ObjCode, CD.LC, Compiler_Data_to_Debug_Info (CD), FCT, B);
  end Emit1;

  procedure Emit2 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    a    :        Operand_1_Type;
    B    :        Operand_2_Type
  )
  is
  begin
    PCode.Emit2 (CD.ObjCode, CD.LC, Compiler_Data_to_Debug_Info (CD), FCT, a, B);
  end Emit2;

  procedure Emit_Std_Funct (
    CD   : in out Compiler_Data;
    Code :        SF_Code
  )
  is
  begin
    Emit1 (CD, k_Standard_Functions, SF_Code'Pos (Code));
  end Emit_Std_Funct;

  procedure Emit_Comparison_Instruction (
    CD        : in out Compiler_Data;
    Operator  :        Comparison_Operator;
    Base_Typ  :        Typen
  )
  is
  begin
    if Base_Typ = Floats then
      case Operator is
        when EQL => Emit (CD, k_EQL_Float);
        when NEQ => Emit (CD, k_NEQ_Float);
        when LSS => Emit (CD, k_LSS_Float);
        when LEQ => Emit (CD, k_LEQ_Float);
        when GTR => Emit (CD, k_GTR_Float);
        when GEQ => Emit (CD, k_GEQ_Float);
      end case;
    elsif Discrete_Typ (Base_Typ) then
      case Operator is
        when EQL => Emit (CD, k_EQL_Integer);
        when NEQ => Emit (CD, k_NEQ_Integer);
        when LSS => Emit (CD, k_LSS_Integer);
        when LEQ => Emit (CD, k_LEQ_Integer);
        when GTR => Emit (CD, k_GTR_Integer);
        when GEQ => Emit (CD, k_GEQ_Integer);
      end case;
    elsif Base_Typ = VStrings then
      case Operator is
        when EQL => Emit (CD, k_EQL_VString);
        when NEQ => Emit (CD, k_NEQ_VString);
        when LSS => Emit (CD, k_LSS_VString);
        when LEQ => Emit (CD, k_LEQ_VString);
        when GTR => Emit (CD, k_GTR_VString);
        when GEQ => Emit (CD, k_GEQ_VString);
      end case;
    else
      raise UErrors.Internal_error with
        "Emit_Comparison_Instruction: comparison instruction for not supported type";
    end if;
  end Emit_Comparison_Instruction;

  procedure Emit_Unary_Minus (
    CD        : in out Compiler_Data;
    Base_Typ  :        Numeric_Typ
  )
  is
  begin
    case Base_Typ is
      when Floats => Emit (CD, k_Unary_MINUS_Float);
      when Ints   => Emit (CD, k_Unary_MINUS_Integer);
    end case;
  end Emit_Unary_Minus;

  procedure Emit_Arithmetic_Binary_Instruction (
    CD        : in out Compiler_Data;
    Operator  :        Arithmetic_Binary_Operator;
    Base_Typ  :        Numeric_Typ
  )
  is
  begin
    case Base_Typ is
      when Floats =>
        case Operator is
          when Plus    => Emit (CD, k_ADD_Float);
          when Minus   => Emit (CD, k_SUBTRACT_Float);
          when Times   => Emit (CD, k_MULT_Float);
          when Divide  => Emit (CD, k_DIV_Float);
          when Power   => Emit (CD, k_Power_Float);
        end case;
      when Ints   =>
        case Operator is
          when Plus    => Emit (CD, k_ADD_Integer);
          when Minus   => Emit (CD, k_SUBTRACT_Integer);
          when Times   => Emit (CD, k_MULT_Integer);
          when Divide  => Emit (CD, k_DIV_Integer);
          when Power   => Emit (CD, k_Power_Integer);
        end case;
    end case;
  end Emit_Arithmetic_Binary_Instruction;

end HAC.Compiler.PCode_Emit;
