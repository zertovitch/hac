with Ada.Strings.Fixed;

package body HAC_Sys.PCode.Interpreter.Exceptions is

  procedure Raise_Standard (
    ND                       : in out In_Defs.Interpreter_Data;
    SE                       :        Exception_Type;
    Msg                      :        String  := "";
    Stop_Current_Instruction :        Boolean := False
  )
  is
    EI : Exception_Propagation_Data renames ND.TCB (ND.CurTask).Exception_Info;
  begin
    EI.Currently_Raised  := (SE, 0);
    EI.Exception_Message := HAT.To_VString (Msg);
    ND.PS := In_Defs.Exception_Raised;
    if Stop_Current_Instruction then
      --  Skip the rest of what the current instruction
      --  does in the run-time library (e.g. I/O operations).
      raise VM_Raised_Exception;
    end if;
  end Raise_Standard;

  procedure Raise_VM_Exception_from_Constraint_Error (CE_Message : String) is
    --  We guess specialized kinds of "Constraint_Error"'s using
    --  the message provided by the host Ada system.
    --  For instance on an overflow check failure, GNAT issues CE with the message
    --  "raised CONSTRAINT_ERROR : xyz.adb:123 overflow check failed".
  begin
    if Ada.Strings.Fixed.Index (CE_Message, "overflow check") > 0 then
      raise VM_Overflow_Error;
    else
      raise VM_Constraint_Error;
    end if;
  end Raise_VM_Exception_from_Constraint_Error;

end HAC_Sys.PCode.Interpreter.Exceptions;
