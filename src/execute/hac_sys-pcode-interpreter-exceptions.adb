package body HAC_Sys.PCode.Interpreter.Exceptions is

  procedure Raise_Standard (
    ND  : in out Interpreter_Data;
    SE  :        Exception_Type;
    Msg :        String := ""
  )
  is
    EI : Exception_Propagation_Data renames ND.TCB (ND.CurTask).Exception_Info;
  begin
    EI.Currently_Raised  := (SE, 0);
    EI.Exception_Message := HAL.To_VString (Msg);
    ND.PS := Exception_Raised;
  end Raise_Standard;

end HAC_Sys.PCode.Interpreter.Exceptions;
