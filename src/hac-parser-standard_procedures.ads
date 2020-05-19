with HAC.Compiler, HAC.PCode;

package HAC.Parser.Standard_Procedures is

  --  NB: Some of the supplied subprograms may disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Procedure (
    CD      : in out Compiler.Compiler_Data;
    Level   :        PCode.Nesting_level;
    FSys    :        Data.Symset;
    Code    :        PCode.SP_Code
  );

end HAC.Parser.Standard_Procedures;
