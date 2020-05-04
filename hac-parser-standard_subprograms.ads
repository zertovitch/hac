with HAC.Compiler;           use HAC.Compiler;
with HAC.PCode;              use HAC.PCode;

package HAC.Parser.Standard_Subprograms is

  use HAC.Data;

  --  NB: Some of the supplied subprograms may disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Function (
    CD          : in out Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    Ident_Index :        Integer;
    Code        :        SF_Code;
    Return_Typ  :    out Exact_Typ
  );

  procedure Standard_Procedure (
    CD      : in out Compiler_Data;
    Level   :        Integer;
    FSys    :        Symset;
    N       :        SP_Code
  );

end HAC.Parser.Standard_Subprograms;
