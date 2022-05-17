-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC_Sys.PCode;

private package HAC_Sys.Parser.Calls is

  procedure Push_and_Check_by_Value_Parameter (
    CD       : in out Co_Defs.Compiler_Data;
    Level    :        Defs.Nesting_level;
    FSys     :        Defs.Symset;
    Expected :        Co_Defs.Exact_Subtyp
  );

  procedure Push_by_Reference_Parameter (
    CD       : in out Co_Defs.Compiler_Data;
    Level    :        Defs.Nesting_level;
    FSys     :        Defs.Symset;
    Found    :    out Co_Defs.Exact_Subtyp
  );

  procedure Entry_Call (
    CD          : in out Co_Defs.Compiler_Data;
    Level       :        Defs.Nesting_level;
    FSys        :        Defs.Symset;
    I           :        Integer;
    CallType    :        PCode.Operand_1_Type
  );

  procedure Subprogram_or_Entry_Call (
    CD          : in out Co_Defs.Compiler_Data;
    Level       :        Defs.Nesting_level;
    FSys        :        Defs.Symset;
    Ident_Index :        Integer;
    CallType    :        PCode.Operand_1_Type
  );

end HAC_Sys.Parser.Calls;
