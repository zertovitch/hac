private package HAC_Sys.Parser.Standard_Procedures is

  --  NB: Some of the supplied subprograms may disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Procedure (
    CD      : in out Co_Defs.Compiler_Data;
    Level   :        Defs.Nesting_Level;
    FSys    :        Defs.Symset;
    Code    :        Defs.SP_Code
  );

end HAC_Sys.Parser.Standard_Procedures;
