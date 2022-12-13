with HAC_Sys.PCode.Interpreter.In_Defs;

package HAC_Sys.PCode.Interpreter.Exceptions is

  ------------------------
  --  HAC's exceptions  --
  ------------------------

  procedure Raise_Standard (
    ND                       : in out In_Defs.Interpreter_Data;
    SE                       :        Exception_Type;
    Msg                      :        String  := "";
    Stop_Current_Instruction :        Boolean := False
  );

  ---------------------------------------------------------
  --  Exceptions raised in the host Ada system during    --
  --  VM execution, leading to a call to Raise_Standard  --
  ---------------------------------------------------------

  VM_Case_Check_Error            : exception;
  VM_Constraint_Error            : exception;
  VM_Division_by_0               : exception;
  VM_End_Error                   : exception;
  VM_Function_End_without_Return : exception;
  VM_Invalid_Data                : exception;
  VM_Out_of_Range                : exception;
  VM_Overflow_Error              : exception;
  VM_Raised_Exception            : exception;  --  See Name_Error for an example.
  VM_Stack_Overflow              : exception;
  VM_Stack_Underflow             : exception;
  VM_Subprogram_Spec             : exception;

  procedure Raise_VM_Exception_from_Constraint_Error (CE_Message : String);

end HAC_Sys.PCode.Interpreter.Exceptions;
