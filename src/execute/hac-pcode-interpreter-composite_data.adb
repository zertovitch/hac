package body HAC.PCode.Interpreter.Composite_Data is

  procedure Do_Composite_Data_Operation (CD : Compiler_Data; ND : in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    IR : Order renames ND.IR;
    H1, H2, H3, H4, H5 : Defs.HAC_Integer;  --  Internal integer registers
    use Defs;

    procedure Do_Array_Index (Element_Size : Index) is
      ATI : constant Integer := IR.Y;
      ATE : ATabEntry renames CD.Arrays_Table (ATI);
      Idx : constant Index := Index (ND.S (Curr_TCB.T).I);
    begin
      if Idx in ATE.Low .. ATE.High then
        Pop (ND);  --  Pull array index, then adjust array element pointer.
        ND.S (Curr_TCB.T).I := ND.S (Curr_TCB.T).I + (Idx - ATE.Low) * Element_Size;
      else
        raise VM_Out_of_Range;
      end if;
    end Do_Array_Index;

    procedure Do_Load_Block is
    begin
      H1 := ND.S (Curr_TCB.T).I;   --  Pull source address
      Pop (ND);
      H2 := IR.Y + Curr_TCB.T;  --  Stack top after pushing block
      if H2 > Curr_TCB.STACKSIZE then
        raise VM_Stack_Overflow;
      else
        while Curr_TCB.T < H2 loop
          Curr_TCB.T := Curr_TCB.T + 1;
          ND.S (Curr_TCB.T) := ND.S (H1);
          H1 := H1 + 1;
        end loop;
      end if;
    end Do_Load_Block;

    procedure Do_Copy_Block is
    begin
      H1 := ND.S (Curr_TCB.T - 1).I;   --  Destination address
      H2 := ND.S (Curr_TCB.T).I;       --  Source address
      H3 := H1 + IR.Y;                 --  IR.Y = block length
      while H1 < H3 loop
        ND.S (H1) := ND.S (H2);
        H1     := H1 + 1;
        H2     := H2 + 1;
      end loop;
      Pop (ND, 2);
    end Do_Copy_Block;

    procedure Do_String_Literal_Assignment is
    begin
      H1 := ND.S (Curr_TCB.T - 2).I;  --  address of array
      H2 := ND.S (Curr_TCB.T).I;      --  index to string table
      H3 := IR.Y;                     --  size of array
      H4 := ND.S (Curr_TCB.T - 1).I;  --  length of string
      if H3 < H4 then
        H5 := H1 + H3;    --  H5 is H1 + min of H3, H4
      else
        H5 := H1 + H4;
      end if;
      while H1 < H5 loop
        --  Copy H5-H1 characters to the stack
        ND.S (H1).I := Character'Pos (CD.Strings_Constants_Table (H2));
        H1 := H1 + 1;
        H2 := H2 + 1;
      end loop;
      --  Padding (!! this will be removed)
      H5 := ND.S (Curr_TCB.T - 2).I + H3;              --  H5 = H1 + H3
      while H1 < H5 loop
        --  fill with blanks if req'd
        ND.S (H1).I := Character'Pos (' ');
        H1 := H1 + 1;
      end loop;
      Pop (ND, 3);
    end Do_String_Literal_Assignment;

  begin
    case Composite_Data_Opcode (ND.IR.F) is
      when k_Array_Index_Element_Size_1 => Do_Array_Index (1);
      when k_Array_Index                => Do_Array_Index (CD.Arrays_Table (IR.Y).Element_Size);
      when k_Record_Field_Offset        => ND.S (Curr_TCB.T).I := ND.S (Curr_TCB.T).I + IR.Y;
      when k_Load_Block                 => Do_Load_Block;
      when k_Copy_Block                 => Do_Copy_Block;
      when k_String_Literal_Assignment  => Do_String_Literal_Assignment;
    end case;
  end Do_Composite_Data_Operation;

end HAC.PCode.Interpreter.Composite_Data;
