package body HAC_Sys.PCode.Interpreter.Exceptions is

  procedure Raise_Standard (
    ND               : in out In_Defs.Interpreter_Data;
    SE               :        Exception_Type;
    Msg              :        String  := "";
    Stop_Instruction :        Boolean := False
  )
  is
    EI : Exception_Propagation_Data renames ND.TCB (ND.CurTask).Exception_Info;
  begin
    EI.Currently_Raised  := (SE, 0);
    EI.Exception_Message := HAL.To_VString (Msg);
    ND.PS := In_Defs.Exception_Raised;
    if Stop_Instruction then
      raise In_Defs.VM_Raised_Exception;
    end if;
  end Raise_Standard;

end HAC_Sys.PCode.Interpreter.Exceptions;
