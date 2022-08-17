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

with HAC_Sys.Errors;

package body HAC_Sys.Compiler.PCode_Emit is

  function Compiler_Data_to_Debug_Info (CD : Compiler_Data) return Debug_Info is
  begin
    return (Line_Number   => CD.CUD.line_count,
            Full_Block_Id => CD.Full_Block_Id,
            File_Name     => CD.CUD.source_file_name);
  end Compiler_Data_to_Debug_Info;

  procedure Emit (
    CD   : in out Compiler_Data;
    FCT  :        Opcode
  )
  is
  begin
    Emit_3 (CD, FCT, 0, 0, 0);
  end Emit;

  procedure Emit_1 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    B    :        Operand_2_Type
  )
  is
  begin
    Emit_3 (CD, FCT, 0, B, 0);
  end Emit_1;

  procedure Emit_2 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    a    :        Operand_1_Type;
    B    :        Operand_2_Type
  )
  is
  begin
    Emit_3 (CD, FCT, a, B, 0);
  end Emit_2;

  procedure Emit_3 (
    CD   : in out Compiler_Data;
    FCT  :        Opcode;
    a    :        Operand_1_Type;
    B    :        Operand_2_Type;
    c    :        Operand_3_Type
  )
  is
    folded, specialized : Boolean;
  begin
    PCode.Emit_Instruction (
      CD.ObjCode (CD.ObjCode'First .. CD.CMax),
      --  ^ We don't pass the full object code table (CD.ObjCode)
      --    but the part before variable initialization code,
      --    for preventing overwriting existing initialization code.
      CD.LC, Compiler_Data_to_Debug_Info (CD), FCT, a, B, c,
      folded,
      specialized
    );
    if folded then
      CD.folded_instructions := CD.folded_instructions + 1;
    end if;
    if specialized then
      CD.specialized_instructions := CD.specialized_instructions + 1;
    end if;
  end Emit_3;

  procedure Emit_Std_Funct (
    CD    : in out Compiler_Data;
    Code  :        SF_Code;
    Extra :        Operand_1_Type := 0
  )
  is
  begin
    Emit_2 (CD, k_HAT_Function, Extra, SF_Code'Pos (Code));
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
      raise Errors.Internal_error with
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

  procedure Emit_Push_Float_Literal (
    CD : in out Compiler_Data;
    X  :        HAC_Float
  )
  is
    RNum_Index : Natural;
  begin
    Enter_or_find_Float (CD, X, RNum_Index);
    Emit_1 (CD, k_Push_Float_Literal, Operand_2_Type (RNum_Index));
  end Emit_Push_Float_Literal;

  procedure Enter_or_find_Float (
    CD         : in out Compiler_Data;
    X          :        HAC_Float;
    RNum_Index :    out Natural
  )
  is
    use Errors;
    use type HAC_Float;
  begin
    if CD.Float_Constants_Count = Float_Const_Table_Max - 1 then
      Fatal (FLOAT_CONSTANTS);  --  Exception is raised there.
    end if;
    --  We add X's value as an extra item: potential new item *and* sentinel value.
    CD.Float_Constants_Table (CD.Float_Constants_Count + 1) := X;
    RNum_Index := 1;
    while CD.Float_Constants_Table (RNum_Index) /= X loop  --  Binary equality.
      RNum_Index := RNum_Index + 1;
    end loop;
    if RNum_Index > CD.Float_Constants_Count then  --  X's value was not previously in the table.
      CD.Float_Constants_Count := RNum_Index;
    end if;
  end Enter_or_find_Float;

end HAC_Sys.Compiler.PCode_Emit;
