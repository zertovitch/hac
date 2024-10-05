private package HAC_Sys.Parser.Standard_Procedures is

  --  NB: Some of the supplied subprograms may disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Procedure
    (CD      : in out Co_Defs.Compiler_Data;
     context : in     Defs.Flow_Context;
     FSys    : in     Defs.Symset;
     Code    : in     Defs.SP_Code);

end HAC_Sys.Parser.Standard_Procedures;
