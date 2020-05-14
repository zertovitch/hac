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

--  This package defines the PCode Virtual Machine.

with HAC.Data;

with Ada.Text_IO;  --  Only used for file descriptors

package HAC.PCode is

  -----------------------------------------------------PCode Opcodes----

  type Opcode is
  (
    k_Push_Address,
    k_Push_Value,
    k_Push_Indirect_Value,
    k_Update_Display_Vector,
    k_Accept_Rendezvous,
    k_End_Rendezvous,
    k_Wait_Semaphore,
    k_Signal_Semaphore,
    k_Standard_Functions,
    k_Record_Field_Offset,
    --
    k_Jump,
    k_Conditional_Jump,
    --
    k_CASE_Switch_1,
    k_CASE_Switch_2,
    k_FOR_Forward_Begin,
    k_FOR_Forward_End,
    k_FOR_Reverse_Begin,
    k_FOR_Reverse_End,
    k_Mark_Stack,                       --  First instruction for a Call
    k_Call,                             --  Procedure and task entry CALL
    k_Array_Index_Element_Size_1,
    k_Array_Index,
    k_Load_Block,
    k_Copy_Block,
    k_Store,
    --
    k_Load_Discrete_Literal,            --  "Load immediate" in some assemblers.
    k_Load_Float_Literal,
    k_String_Literal_Assignment,
    --
    k_Integer_to_Float,                 --  The reverse conversion is done by a k_Standard_Functions
    --
    k_Exit_Call,
    k_Exit_Function,
    k_Dereference,
    --
    k_Unary_MINUS_Float,                --  2020-04-04
    k_Unary_MINUS_Integer,
    k_NOT_Boolean,
    --
    k_EQL_Integer,
    k_NEQ_Integer,
    k_LSS_Integer,
    k_LEQ_Integer,
    k_GTR_Integer,
    k_GEQ_Integer,
    --
    k_EQL_VString,
    k_NEQ_VString,
    k_LSS_VString,
    k_LEQ_VString,
    k_GTR_VString,
    k_GEQ_VString,
    --
    k_EQL_Float,
    k_NEQ_Float,
    k_LSS_Float,
    k_LEQ_Float,
    k_GTR_Float,
    k_GEQ_Float,
    --
    k_ADD_Integer,
    k_SUBTRACT_Integer,
    k_MULT_Integer,
    k_DIV_Integer,
    k_MOD_Integer,
    k_Power_Integer,                    --  2018-03-18 : 3 ** 6
    --
    k_ADD_Float,
    k_SUBTRACT_Float,
    k_MULT_Float,
    k_DIV_Float,
    k_Power_Float,                      --  2018-03-22 : 3.14 ** 6.28
    k_Power_Float_Integer,              --  2018-03-22 : 3.14 ** 6
    --
    k_OR_Boolean,
    k_AND_Boolean,
    k_XOR_Boolean,
    --
    k_Read,
    k_Write_String_Literal,
    k_Write_Formatted,
    k_Skip_Line,
    k_New_Line,
    k_File_I_O,
    --
    k_Halt_Interpreter,                 --  Switch off the processor's running loop
    k_Delay,
    k_Cursor_At,
    k_Set_Quantum_Task,
    k_Set_Task_Priority,
    k_Set_Task_Priority_Inheritance,
    k_Selective_Wait
  );

  subtype Jump_Opcode is Opcode range k_Jump .. k_Conditional_Jump;
  subtype Binary_Operator_Opcode is Opcode range k_EQL_Integer .. k_XOR_Boolean;
  subtype Unary_Operator_Opcode  is Opcode range k_Unary_MINUS_Float .. k_NOT_Boolean;

  function For_END (for_BEGIN: Opcode) return Opcode;

  subtype Operand_1_Type is Integer;       -- was -LMax..+LMax (levels)

  --  Type for operand 2 (Y) is large enough for containing
  --  addresses, plus signed integer values *in* HAC programs.
  --
  subtype Operand_2_Type is HAC.Data.HAC_Integer;

  type Debug_Info is record
    Line  : Positive;       --  Line number in the source code.
    Block : HAC.Data.Alfa;  --  Current block's identifier (if any).
    --  Unit  : HAC.Data.Alfa;  --  Compilation unit identifier.
  end record;

  --  PCode instruction record (stores a compiled PCode instruction)
  type Order is record
    F : Opcode;          --  Opcode (or instruction field)
    X : Operand_1_Type;  --  Operand 1 is mostly used to point to the static level
    Y : Operand_2_Type;  --  Operand 2 is used to pass addresses and sizes to the
                         --    instructions or immediate discrete values (k_Literal).
    D : Debug_Info;
  end record;

  type Object_Code_Table is array (Natural range <>) of Order;

  dummy_address : constant := -1;
  --  For jumps forward in the code towards an ELSE, ELSIF, END IF, END LOOP, ...
  --  When the code is emited, the address is still unknown.
  --  When the address is known, jump addresses are patched.

  --  Patch to OC'Last all addresses of Jump_Opcode's which are equal to dummy_address.
  procedure Patch_Addresses (OC : in out Object_Code_Table);

  --  Mechanism for patching instructions at selected addresses.
  type Patch_Table is array (Positive range <>) of Operand_2_Type;
  subtype Fixed_Size_Patch_Table is Patch_Table (1 .. HAC.Data.Patch_Max);

  --  Patch to OC'Last all addresses for Jump instructions whose
  --  addresses are contained in the Patch_Table, up to index Top.
  --  Reset Top to 0.
  procedure Patch_Addresses (
    OC  : in out Object_Code_Table;
    PT  :        Patch_Table;
    Top : in out Natural
  );

  --  Add new instruction address to a Patch_Table.
  procedure Feed_Patch_Table (
    PT  : in out Patch_Table;
    Top : in out Natural;
    LC  :        Integer
  );

  procedure Dump (OC : Object_Code_Table; Text : Ada.Text_IO.File_Type);

  --  Store PCode instruction in the object code table OC at position LC and increments LC.
  procedure Emit (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    D    :        Debug_Info;
    FCT  :        Opcode);

  procedure Emit1 (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    D    :        Debug_Info;
    FCT  :        Opcode;
    B    :        Operand_2_Type
  );

  procedure Emit2 (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    D    :        Debug_Info;
    FCT  :        Opcode;
    a    :        Operand_1_Type;
    B    :        Operand_2_Type
  );

  --  Save and restore an object file
  procedure SaveOBJ (FileName: String);
  procedure RestoreOBJ (FileName: String);

  ------------------------------------
  --  Standard function operations  --
  ------------------------------------

  type SF_Code is (
    SF_Abs_Int,
    SF_Abs_Float,
    SF_T_Val,                   --  S'Val  : RM 3.5.5 (5)
    SF_T_Pos,                   --  S'Pos  : RM 3.5.5 (2)
    SF_T_Succ,                  --  S'Succ : RM 3.5 (22)
    SF_T_Pred,                  --  S'Pred : RM 3.5 (25)
    --  Numerical functions
    SF_Round_Float_to_Int,
    SF_Trunc_Float_to_Int,
    SF_Sin,
    SF_Cos,
    SF_Exp,
    SF_Log,
    SF_Sqrt,
    SF_Arctan,
    SF_EOF,
    SF_EOLN,
    SF_Random_Int,
    --  VString functions
    SF_Literal_to_VString,      --  +"Hello"
    SF_Two_VStrings_Concat,     --  V1 & V2
    SF_VString_Char_Concat,     --  V & 'x'
    SF_Char_VString_Concat,     --  'x' & V
    SF_LStr_VString_Concat,     --  "Hello " & V
    --
    SF_VString_Int_Concat,      --  V & 123
    SF_Int_VString_Concat,      --  123 & V
    SF_VString_Float_Concat,    --  V & 3.14159
    SF_Float_VString_Concat,    --  3.14159 & V
    SF_Element,
    SF_Length,
    SF_Slice,
    SF_To_Lower_Char,
    SF_To_Upper_Char,
    SF_To_Lower_VStr,
    SF_To_Upper_VStr,
    SF_Index,
    SF_Int_Times_Char,
    SF_Int_Times_VStr,
    SF_Trim_Left,
    SF_Trim_Right,
    SF_Trim_Both,
    SF_Image_Ints,
    SF_Image_Floats,            --  "Nice" image
    SF_Image_Attribute_Floats,  --  Image attribute "as is"
    SF_Integer_Value,
    SF_Float_Value,
    --
    SF_Argument,
    SF_Get_Env,
    SF_Shell_Execute,
    --
    --  Niladic functions.
    --
    SF_Clock,
    SF_Random_Float,
    SF_Argument_Count,
    SF_Get_Needs_Skip_Line  --  Informs whether Get from console needs Skip_Line
  );

  subtype SF_Niladic is
    SF_Code range SF_Clock .. SF_Get_Needs_Skip_Line;

  -------------------------------------
  --  Standard procedure operations  --
  -------------------------------------

  type SP_Code is (
    SP_Get,
    SP_Get_Immediate,
    SP_Get_Line,
    SP_Skip_Line,
    SP_Put,
    SP_Put_Line,
    SP_New_Line,
    SP_Wait,
    SP_Signal,
    SP_Reset,
    SP_Rewrite,
    SP_Close,
    SP_Push_Abstract_Console,
    --
    SP_CursorAt,
    SP_Quantum,
    SP_Priority,
    SP_InheritP,
    SP_Set_Env
  );

end HAC.PCode;
