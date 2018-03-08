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

--  This unit is used to store object codes in the ObjCode table.
--  The three procedures Emit, Emit1, and Emit2 are called from
--  the compiler and the parser.

package HAC.PCode is

  dummy_address : constant := -1;
  --  For jumps forward in the code towards an ELSE, ELSIF, END IF, END LOOP, ...
  --  When the code is emited, the address is still unknown.

  --  Store PCode object in the object code table
  procedure Emit(FCT: Integer);
  procedure Emit1(FCT, B: Integer);
  procedure Emit2(FCT, a, B: Integer);

  -----------------------------------------------------PCode Opcodes----

  k_Load_Address              : constant := 0;
  k_Push_Value                : constant := 1;
  k_Push_Indirect_Value       : constant := 2;
  k_Update_Display_Vector     : constant := 3;
  k_Accept_Rendezvous         : constant := 4;
  k_End_Rendezvous            : constant := 5;
  k_Wait_Semaphore            : constant := 6;
  k_Signal_Semaphore          : constant := 7;
  k_Standard_Functions        : constant := 8;
  k_Offset                    : constant := 9;
  k_Jump                      : constant := 10;
  k_Conditional_Jump          : constant := 11;
  kSwitch                     : constant := 12;
  k_Switch_2                  : constant := 13;
  kFor1                       : constant := 14;
  kFor2                       : constant := 15;
  kFor1Rev                    : constant := 16;
  kFor2Rev                    : constant := 17;
  kMarkStack                  : constant := 18;
  kCall                       : constant := 19; -- procedure and task entry CALL
  kIndex1                     : constant := 20;
  kIndex                      : constant := 21;
  k_Load_Block                : constant := 22;
  k_Copy_Block                : constant := 23;
  k_Literal                   : constant := 24;
  k_Load_Float                : constant := 25;
  k_Integer_to_Float          : constant := 26;
  k_Read                      : constant := 27;
  k_Write_String              : constant := 28;
  kWrite1                     : constant := 29;
  kWrite2                     : constant := 30;
  k_Exit_Call                 : constant := 32;
  k_Exit_Function             : constant := 33;
  kCase34                     : constant := 34;
  k_NOT_Boolean               : constant := 35;
  k_Unary_MINUS_Integer       : constant := 36;
  k_Write_Float               : constant := 37;
  k_Store                     : constant := 38;
  k_EQL_Float                 : constant := 39;
  k_NEQ_Float                 : constant := 40;
  k_LSS_Float                 : constant := 41;
  k_LEQ_Float                 : constant := 42;
  k_GTR_Float                 : constant := 43;
  k_GEQ_Float                 : constant := 44;
  k_EQL_Integer               : constant := 45;
  k_NEQ_Integer               : constant := 46;
  k_LSS_Integer               : constant := 47;
  k_LEQ_Integer               : constant := 48;
  k_GTR_Integer               : constant := 49;
  k_GEQ_Integer               : constant := 50;
  k_OR_Boolean                : constant := 51;
  k_ADD_Integer               : constant := 52;
  k_SUBTRACT_Integer          : constant := 53;
  k_ADD_Float                 : constant := 54;
  k_SUBTRACT_Float            : constant := 55;
  k_AND_Boolean               : constant := 56;
  k_MULT_Integer              : constant := 57;
  k_DIV_Integer               : constant := 58;
  k_MOD_Integer               : constant := 59;
  k_MULT_Float                : constant := 60;
  k_DIV_Float                 : constant := 61;
  kGetNewline                 : constant := 62;
  kPutNewline                 : constant := 63;
  k_Set_current_file_pointer  : constant := 64;
  kFile_I_O                   : constant := 65;
  k_Halt_Interpreter          : constant := 66;
  k_String_assignment         : constant := 67;
  k_Delay                     : constant := 68;
  kCursorAt                   : constant := 69;
  kSetQuatumTask              : constant := 70;
  kSetTaskPriority            : constant := 71;
  kSetTaskPriorityInheritance : constant := 72;
  kSelectiveWait              : constant := 73;
  kHighlightSource            : constant := 74;
  k_XOR_Boolean               : constant := 75;

  --  Save and restore an object file
  procedure SaveOBJ(FileName: String);
  procedure RestoreOBJ(FileName: String);

end HAC.PCode;
