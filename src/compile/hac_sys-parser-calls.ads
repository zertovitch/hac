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

  procedure Push_Parameter_by_Value
    (CD       : in out Co_Defs.Compiler_Data;
     context  :        Defs.Flow_Context;
     fsys     :        Defs.Symset;
     expected :        Co_Defs.Exact_Subtyp);

  procedure Push_Parameter_by_Reference
    (CD       : in out Co_Defs.Compiler_Data;
     context  :        Defs.Flow_Context;
     fsys     :        Defs.Symset;
     name     :        String;
     mode     :        Co_Defs.Parameter_Kind;
     found    :    out Co_Defs.Exact_Subtyp);

  procedure Entry_Call
    (CD          : in out Co_Defs.Compiler_Data;
     context     :        Defs.Flow_Context;
     fsys        :        Defs.Symset;
     i           :        Integer;
     call_type   :        PCode.Operand_1_Type);

  procedure Subprogram_or_Entry_Call
    (CD          : in out Co_Defs.Compiler_Data;
     context     :        Defs.Flow_Context;
     fsys        :        Defs.Symset;
     ident_index :        Integer;
     call_type   :        PCode.Operand_1_Type);

end HAC_Sys.Parser.Calls;
