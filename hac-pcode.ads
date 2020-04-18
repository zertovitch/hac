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

with Ada.Text_IO;

package HAC.PCode is

  -----------------------------------------------------PCode Opcodes----

  type Opcode is
  (
    k_Load_Address,
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
    k_Literal,                          --  "Load immediate" in some assemblers.
    k_Load_Float,
    k_Integer_to_Float,
    --
    k_Read,
    k_Write_String,
    k_Write_1,
    k_Write_2,
    k_Write_Float,
    --
    k_Exit_Call,
    k_Exit_Function,
    k_Case_34,                          --  The instruction #34: "stack_top := (stack_top.I).all"
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
    k_Get_Newline,
    k_Put_Newline,
    k_Set_current_file_pointer,
    k_File_I_O,
    k_Halt_Interpreter,                 --  Switch off the processor's running loop
    k_String_assignment,
    k_Delay,
    k_Cursor_At,
    k_Set_Quantum_Task,
    k_Set_Task_Priority,
    k_Set_Task_Priority_Inheritance,
    k_Selective_Wait,
    k_Highlight_Source
  );

  subtype Jump_Opcode is Opcode range k_Jump .. k_Conditional_Jump;
  subtype Binary_Operator_Opcode is Opcode range k_EQL_Integer .. k_XOR_Boolean;
  subtype Unary_Operator_Opcode  is Opcode range k_Unary_MINUS_Float .. k_NOT_Boolean;

  function For_END (for_BEGIN: Opcode) return Opcode;

  subtype Operand1 is Integer;       -- was -LMax..+LMax (levels)
  subtype Operand2 is Integer;  --  !! TBD: set it to a 64-bit signed.

  --  PCode instruction record (stores a compiled PCode instruction)
  type Order is record
    F : Opcode;    --  Opcode (or instruction field)
    X : Operand1;  --  Operand 1 is used to point to the static level
    Y : Operand2;  --  Operand 2 is used to pass operands to the instructions
                   --    or immediate discrete values (k_Literal).
  end record;

  type Object_Code_Table is array (Natural range <>) of Order;

  dummy_address : constant := -1;
  --  For jumps forward in the code towards an ELSE, ELSIF, END IF, END LOOP, ...
  --  When the code is emited, the address is still unknown.
  --  When the address is known, jump addresses are patched.

  --  Patch all addresses which are = dummy_address to OC'Last.
  procedure Patch_Addresses (OC : in out Object_Code_Table);

  procedure Dump (OC : Object_Code_Table; Text : Ada.Text_IO.File_Type);

  --  Store PCode instruction in the object code table OC at position LC and increments LC.
  procedure Emit (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    FCT  :        Opcode);
  procedure Emit1 (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    FCT  :        Opcode;
    B    :        Integer);
  procedure Emit2 (
    OC   : in out Object_Code_Table;
    LC   : in out Integer;
    FCT  :        Opcode;
    a, B :        Integer);

  --  Save and restore an object file
  procedure SaveOBJ (FileName: String);
  procedure RestoreOBJ (FileName: String);

  --  Standard function operations

  SF_Abs                : constant :=  0;
  SF_T_Val              : constant :=  5;  --  S'Val : RM 3.5.5 (5)
  SF_T_Pos              : constant :=  6;  --  S'Pos : RM 3.5.5 (2)
  SF_T_Succ             : constant :=  7;  --  S'Succ : RM 3.5 (22)
  SF_T_Pred             : constant :=  8;  --  S'Pred : RM 3.5 (25)
  SF_Round_Float_to_Int : constant :=  9;
  SF_Trunc_Float_to_Int : constant := 10;
  SF_Sin                : constant := 11;
  SF_Cos                : constant := 12;
  SF_Exp                : constant := 13;
  SF_Log                : constant := 14;
  SF_Sqrt               : constant := 15;
  SF_Arctan             : constant := 16;
  SF_EOF                : constant := 17;
  SF_EOLN               : constant := 18;
  SF_Random             : constant := 19;
  SF_Clock              : constant := 100;

end HAC.PCode;
