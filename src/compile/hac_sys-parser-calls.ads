with HAC_Sys.Defs,
     HAC_Sys.PCode;

private package HAC_Sys.Parser.Calls is

  use Defs;

  procedure Push_and_Check_by_Value_Parameter (
    CD       : in out Compiler_Data;
    Level    :        Nesting_level;
    FSys     :        Symset;
    Expected :        Exact_Typ
  );

  procedure Push_by_Reference_Parameter (
    CD       : in out Compiler_Data;
    Level    :        Nesting_level;
    FSys     :        Symset;
    Found    :    out Exact_Typ
  );

  procedure Entry_Call (
    CD          : in out Compiler_Data;
    Level       :        Nesting_level;
    FSys        :        Symset;
    I           :        Integer;
    CallType    :        PCode.Operand_1_Type
  );

  procedure Subprogram_or_Entry_Call (
    CD          : in out Compiler_Data;
    Level       :        Nesting_level;
    FSys        :        Symset;
    I           :        Integer;
    CallType    :        PCode.Operand_1_Type
  );

end HAC_Sys.Parser.Calls;
