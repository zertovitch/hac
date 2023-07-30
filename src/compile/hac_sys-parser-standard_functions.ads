private package HAC_Sys.Parser.Standard_Functions is

  --  NB: Some of the supplied subprograms may disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Function (
    CD          : in out Co_Defs.Compiler_Data;
    Level       :        Defs.Nesting_Level;
    FSys        :        Defs.Symset;
    Ident_Index :        Integer;
    Code        :        Defs.SF_Code;
    Return_Typ  :    out Co_Defs.Exact_Subtyp
  );

end HAC_Sys.Parser.Standard_Functions;
