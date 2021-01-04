with HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Enter_Def is

  use Helpers, UErrors;

  ------------------------------------------------------------------
  ------------------------------------------------------Enter_Block-
  procedure Enter_Block (
    CD    : in out Compiler_Data;
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
    CD               : in out Compiler_Data;
    Level            :        PCode.Nesting_level;
    Id, Id_with_case :        Defs.Alfa;
    K                :        Entity_Kind
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
         (Name           => Id,
          Name_with_case => Id_with_case,
          Link           => L,
          Entity         => K,
          Read_only      => False,
          xTyp           => Type_Undefined,
          Block_Ref      => 0,
          Normal         => True,
          LEV            => Level,
          Adr_or_Sz      => 0,
          Discrete_First => 0,
          Discrete_Last  => 0
         );
      --  Update start of identifier chain:
      CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx := CD.Id_Count;
    end if;
  end Enter;

  ------------------------------------------------------------------
  -------------------------------------------------------EnterArray-

  procedure Enter_Array (
    CD       : in out Compiler_Data;
    Index_TP :        Exact_Typ;
    L, H     :        Integer
  )
  is
  begin
    if L > H then
      Error (CD,
        err_illegal_array_bounds, "Low > High. NB: legal in Ada (empty array)", -- !!
        stop => True
      );
    end if;
    if abs (L) > XMax or abs (H) > XMax then
      Error (CD,
        err_illegal_array_bounds, "absolute value of a bound exceeds maximum value",
        stop => True
      );
    end if;
    if CD.Arrays_Count = AMax then
      Fatal (ARRAYS);  --  Exception is raised there.
    end if;
    CD.Arrays_Count := CD.Arrays_Count + 1;
    declare
      New_A : ATabEntry renames CD.Arrays_Table (CD.Arrays_Count);
    begin
      New_A.Index_xTyp := Index_TP;
      New_A.Low        := L;
      New_A.High       := H;
    end;
  end Enter_Array;

  ------------------------------------------------------------------
  --------------------------------------------------Enter_Variables-

  procedure Enter_Variables (
    CD    : in out Compiler_Data;
    Level :        PCode.Nesting_level
  )
  is
    procedure Enter_Variable is
    begin
      if CD.Sy = IDent then
        Enter (CD, Level, CD.Id, CD.Id_with_case, Variable);
        Scanner.InSymbol (CD);
      else
        Error (CD, err_identifier_missing);
      end if;
    end Enter_Variable;
    --
  begin
    Enter_Variable;
    while CD.Sy = Comma loop  --  ','  in  "a, b, c : Integer;"
      Scanner.InSymbol (CD);
      Enter_Variable;
    end loop;
  end Enter_Variables;

end HAC_Sys.Parser.Enter_Def;
