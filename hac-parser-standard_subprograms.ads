with HAC.Compiler;           use HAC.Compiler;

package HAC.Parser.Standard_Subprograms is

  use HAC.Data;

  --  NB: Most of this part will disappear when modularity,
  --  Ada.Text_IO etc. will be implemented, as well as overloading.

  procedure Standard_Function (
    CD          : in out Compiler_Data;
    Level       :        Integer;
    FSys        :        Symset;
    Ident_Index :        Integer;
    SF_Code     :        Integer;
    X           :    out Exact_Typ
  );

  procedure Standard_Procedure (
    CD      : in out Compiler_Data;
    Level   :        Integer;
    FSys    :        Symset;
    N       :        Integer
  );

end HAC.Parser.Standard_Subprograms;