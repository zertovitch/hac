with HAC.Data; use HAC.Data;

package HAC.Parser.Calls is

  procedure Entry_Call (
    CD          : in out HAC.Compiler.Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    I, CallType :        Integer
  );

  procedure Subprogram_or_Entry_Call (
    CD          : in out HAC.Compiler.Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    I, CallType :        Integer
  );

end HAC.Parser.Calls;
