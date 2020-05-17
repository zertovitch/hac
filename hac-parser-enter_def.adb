with HAC.Parser.Helpers,
     HAC.UErrors;

package body HAC.Parser.Enter_Def is

  use Compiler, Data, Helpers, UErrors;

  ------------------------------------------------------------------
  ------------------------------------------------------Enter_Block-
  procedure Enter_Block (
    CD    : in out Compiler.Compiler_Data;
    Tptr  :        Integer
  )
  is
  begin
    if CD.Blocks_Count = BMax then
      Fatal (PROCEDURES);  --  Exception is raised there.
    end if;
    CD.Blocks_Count := CD.Blocks_Count + 1;
    declare
      New_B : BTabEntry renames CD.Blocks_Table (CD.Blocks_Count);
    begin
      New_B.Id                := CD.IdTab (Tptr).Name;
      New_B.Last_Id_Idx       := 0;
      New_B.Last_Param_Id_Idx := 0;
      New_B.SrcFrom           := CD.Line_Count;
    end;
  end Enter_Block;

  ------------------------------------------------------------------
  ------------------------------------------------------------Enter-
  procedure Enter (
    CD               : in out Compiler.Compiler_Data;
    Level            :        PCode.Nesting_level;
    Id, Id_with_case :        Data.Alfa;
    K                :        Compiler.aObject
  )
  is
    J, L : Integer;
  begin
    if CD.Id_Count = Id_Table_Max then
      Fatal (IDENTIFIERS);  --  Exception is raised there.
    end if;
    CD.IdTab (No_Id).Name := Id;  --  Sentinel
    J                     := CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx;
    L                     := J;
    while CD.IdTab (J).Name /= Id loop
      J := CD.IdTab (J).Link;
    end loop;
    --  Follow the chain of identifiers for current Level.
    if J /= No_Id then
      Error (CD, err_duplicate_identifier, To_String (Id));
    else      --  Enter identifier in table IdTab
      CD.Id_Count            := CD.Id_Count + 1;
      CD.IdTab (CD.Id_Count) :=
       (  Name           => Id,
          Name_with_case => Id_with_case,
          Link           => L,
          Obj            => K,
          Read_only      => False,
          xTyp           => (TYP => NOTYP, Ref => 0),
          Block_Ref      => 0,
          Normal         => True,
          LEV            => Level,
          Adr_or_Sz      => 0
      );
      --  Update start of identifier chain:
      CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx := CD.Id_Count;
    end if;
  end Enter;

end HAC.Parser.Enter_Def;
