with HAC_Sys.PCode.Interpreter.In_Defs;

package HAC_Sys.pcode.interpreter.Exceptions is

  use In_Defs;

  procedure Raise_Standard (
    ND  : in out Interpreter_Data;
    SE  :        Exception_Type;
    Msg :        String := ""
  );

end HAC_Sys.PCode.Interpreter.Exceptions;
