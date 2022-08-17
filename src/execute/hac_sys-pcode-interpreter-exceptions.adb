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
      raise In_Defs.VM_Raised_Exception;
    end if;
  end Raise_Standard;

end HAC_Sys.PCode.Interpreter.Exceptions;
