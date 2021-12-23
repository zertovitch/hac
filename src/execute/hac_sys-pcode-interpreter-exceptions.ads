with HAC_Sys.PCode.Interpreter.In_Defs;

package HAC_Sys.PCode.Interpreter.Exceptions is

  procedure Raise_Standard (
    ND                       : in out In_Defs.Interpreter_Data;
    SE                       :        Exception_Type;
    Msg                      :        String  := "";
    Stop_Current_Instruction :        Boolean := False
  );

end HAC_Sys.PCode.Interpreter.Exceptions;
