with HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Enter_Def is

  use Co_Defs, Defs, UErrors;

  ------------------------------------------------------------------
  ------------------------------------------------------Enter_Block-
  procedure Enter_Block (
    CD    : in out Co_Defs.Compiler_Data;
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
      New_B.SrcFrom           := CD.CUD.line_count;
    end;
  end Enter_Block;

  ------------------------------------------------------------------
  ------------------------------------------------------------Enter-
  procedure Enter (
    CD               : in out Co_Defs.Compiler_Data;
    Level            :        Defs.Nesting_level;
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
    if J = No_Id then
      --  Enter identifier in table IdTab
      CD.Id_Count            := CD.Id_Count + 1;
      CD.IdTab (CD.Id_Count) :=
         (Name           => Id,
          Name_with_case => Id_with_case,
          Link           => L,
          Entity         => K,
          Read_only      => False,
          xTyp           => Undefined,
          Block_Ref      => 0,
          Normal         => True,
          LEV            => Level,
          Adr_or_Sz      => 0
         );
      --  Update start of identifier chain:
      CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx := CD.Id_Count;
      if Level = 0 then
        CD.CUD.level_0_def.Include (Id);
      end if;
    else
      Error (CD, err_duplicate_identifier, To_String (Id), major);
    end if;
  end Enter;

  ------------------------------------------------------------------
  -------------------------------------------------------EnterArray-

  procedure Enter_Array (
    CD        : in out Co_Defs.Compiler_Data;
    Index_STP :        Co_Defs.Exact_Subtyp
  )
  is
    use type HAC_Integer;
  begin
    if Index_STP.Discrete_First > Index_STP.Discrete_Last then
      Error (CD,
        err_illegal_array_bounds, "Low > High. NB: legal in Ada (empty array)", -- !!
        major
      );
    end if;
    if   abs (Index_STP.Discrete_First) > HAC_Integer (XMax)
      or abs (Index_STP.Discrete_Last)  > HAC_Integer (XMax)
    then
      Error (CD,
        err_illegal_array_bounds, "absolute value of a bound exceeds maximum value",
        major
      );
    end if;
    if CD.Arrays_Count = AMax then
      Fatal (ARRAYS);  --  Exception is raised there.
    end if;
    CD.Arrays_Count := CD.Arrays_Count + 1;
    CD.Arrays_Table (CD.Arrays_Count).Index_xTyp := Index_STP;
  end Enter_Array;

  ------------------------------------------------------------------
  --------------------------------------------------Enter_Variables-

  procedure Enter_Variables (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level
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
