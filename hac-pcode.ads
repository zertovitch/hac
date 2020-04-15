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

package HAC.PCode is

  type Opcode is range 0 .. 79;
  subtype Operand1 is Integer;       -- was -LMax..+LMax (levels)
  subtype Operand2 is Integer;

  --  PCode instruction record (stores a compiled PCode instruction)
  type Order is record
    F : HAC.PCode.Opcode;  --  Opcode (or instruction field)
    X : Operand1;          --  Operand 1 is used to point to the static level
    Y : Operand2;          --  Operand 2 is used to pass operands to the instructions
  end record;

  type Object_Code_Table is array (Natural range <>) of Order;

  dummy_address : constant := -1;
  --  For jumps forward in the code towards an ELSE, ELSIF, END IF, END LOOP, ...
  --  When the code is emited, the address is still unknown.
  --  When the address is known, jump addresses are patched.

  --  Patch all addresses which are = dummy_address to LC_Current.
  procedure Patch_Addresses (OC : in out Object_Code_Table; LC_From, LC_Current : Integer);

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

  -----------------------------------------------------PCode Opcodes----

  k_Load_Address                  : constant := 0;
  k_Push_Value                    : constant := 1;
  k_Push_Indirect_Value           : constant := 2;
  k_Update_Display_Vector         : constant := 3;
  k_Accept_Rendezvous             : constant := 4;
  k_End_Rendezvous                : constant := 5;
  k_Wait_Semaphore                : constant := 6;
  k_Signal_Semaphore              : constant := 7;
  k_Standard_Functions            : constant := 8;
  k_Offset                        : constant := 9;
  k_Jump                          : constant := 10;
  k_Conditional_Jump              : constant := 11;
  k_CASE_Switch_1                 : constant := 12;  --  For CASE statement
  k_CASE_Switch_2                 : constant := 13;  --  For CASE statement
  kFor1                           : constant := 14;
  kFor2                           : constant := 15;
  kFor1Rev                        : constant := 16;
  kFor2Rev                        : constant := 17;
  kMarkStack                      : constant := 18;
  kCall                           : constant := 19;  --  procedure and task entry CALL
  kIndex1                         : constant := 20;
  kIndex                          : constant := 21;
  k_Load_Block                    : constant := 22;
  k_Copy_Block                    : constant := 23;
  k_Literal                       : constant := 24;
  k_Load_Float                    : constant := 25;
  k_Integer_to_Float              : constant := 26;
  k_Read                          : constant := 27;
  k_Write_String                  : constant := 28;
  kWrite1                         : constant := 29;
  kWrite2                         : constant := 30;
  k_NOP                           : constant := 31;  --  Added 2020-04-04 for filling the gap...
  k_Exit_Call                     : constant := 32;
  k_Exit_Function                 : constant := 33;
  kCase34                         : constant := 34;
  k_NOT_Boolean                   : constant := 35;
  k_Unary_MINUS_Integer           : constant := 36;
  k_Write_Float                   : constant := 37;
  k_Store                         : constant := 38;
  k_EQL_Float                     : constant := 39;
  k_NEQ_Float                     : constant := 40;
  k_LSS_Float                     : constant := 41;
  k_LEQ_Float                     : constant := 42;
  k_GTR_Float                     : constant := 43;
  k_GEQ_Float                     : constant := 44;
  k_EQL_Integer                   : constant := 45;
  k_NEQ_Integer                   : constant := 46;
  k_LSS_Integer                   : constant := 47;
  k_LEQ_Integer                   : constant := 48;
  k_GTR_Integer                   : constant := 49;
  k_GEQ_Integer                   : constant := 50;
  k_OR_Boolean                    : constant := 51;
  k_ADD_Integer                   : constant := 52;
  k_SUBTRACT_Integer              : constant := 53;
  k_ADD_Float                     : constant := 54;
  k_SUBTRACT_Float                : constant := 55;
  k_AND_Boolean                   : constant := 56;
  k_MULT_Integer                  : constant := 57;
  k_DIV_Integer                   : constant := 58;
  k_MOD_Integer                   : constant := 59;
  k_MULT_Float                    : constant := 60;
  k_DIV_Float                     : constant := 61;
  kGetNewline                     : constant := 62;
  kPutNewline                     : constant := 63;
  k_Set_current_file_pointer      : constant := 64;
  kFile_I_O                       : constant := 65;
  k_Halt_Interpreter              : constant := 66;  --  Switch off the processor's running loop
  k_String_assignment             : constant := 67;
  k_Delay                         : constant := 68;
  k_Cursor_At                     : constant := 69;
  k_Set_Quantum_Task              : constant := 70;
  k_Set_Task_Priority             : constant := 71;
  k_Set_Task_Priority_Inheritance : constant := 72;
  k_Selective_Wait                : constant := 73;
  kHighlightSource                : constant := 74;
  k_XOR_Boolean                   : constant := 75;
  k_Power_Integer                 : constant := 76;  --  2018-03-18 : 3 ** 6
  k_Power_Float_Integer           : constant := 77;  --  2018-03-22 : 3.14 ** 6
  k_Power_Float                   : constant := 78;  --  2018-03-22 : 3.14 ** 6.28
  k_Unary_MINUS_Float             : constant := 79;  --  2020-04-04

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
