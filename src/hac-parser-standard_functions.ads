with HAC.Compiler, HAC.PCode;

private package HAC.Parser.Standard_Functions is

  --  NB: Some of the supplied subprograms may disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Function (
    CD          : in out Compiler.Compiler_Data;
    Level       :        PCode.Nesting_level;
    FSys        :        Data.Symset;
    Ident_Index :        Integer;
    Code        :        PCode.SF_Code;
    Return_Typ  :    out Compiler.Exact_Typ
  );

end HAC.Parser.Standard_Functions;
