with HAC.Compiler;           use HAC.Compiler;
with HAC.Data;               use HAC.Data;

package HAC.Parser.Calls is

  procedure Push_and_Check_by_Value_Parameter (
    CD       : in out HAC.Compiler.Compiler_Data;
    Level    :        Integer;
    FSys     :        Symset;
    Expected :        Exact_Typ
  );

  procedure Push_by_Reference_Parameter (
    CD       : in out HAC.Compiler.Compiler_Data;
    Level    :        Integer;
    FSys     :        Symset;
    Found    :    out Exact_Typ
  );

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
