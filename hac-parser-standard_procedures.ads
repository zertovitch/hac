with HAC.Compiler;           use HAC.Compiler;
with HAC.PCode;              use HAC.PCode;

package HAC.Parser.Standard_Procedures is

  use HAC.Data;

  --  NB: Some of the supplied subprograms may disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Procedure (
    CD      : in out Compiler_Data;
    Level   :        Integer;
    FSys    :        Symset;
    Code    :        SP_Code
  );

end HAC.Parser.Standard_Procedures;
