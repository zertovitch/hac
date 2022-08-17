with HAC_Sys.Scanner,
     HAC_Sys.Errors;

with HAT;

package body HAC_Sys.Parser.Enter_Def is

  use Co_Defs, Defs, Errors;
  use type HAC_Integer;

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
      New_B.Id                 := CD.IdTab (Tptr).name;
      New_B.Last_Id_Idx        := 0;
      New_B.First_Param_Id_Idx := 0;
      New_B.Last_Param_Id_Idx  := 0;
      New_B.SrcFrom            := CD.CUD.line_count;
    end;
  end Enter_Block;

  ------------------------------------------------------------------
  ------------------------------------------------------------Enter-
  procedure Enter (
    CD               : in out Co_Defs.Compiler_Data;
    Level            :        Defs.Nesting_level;
    Id, Id_with_case :        Defs.Alfa;
    K                :        Entity_Kind;
    Forward_Decl_Id  :    out Natural
  )
  is
    last_id : constant Index :=
      CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx;
    J : Integer := last_id;
    use HAT;
    prefixed_Id           : constant Alfa := CD.pkg_prefix & Id;
    prefixed_Id_with_case : constant Alfa := CD.pkg_prefix & Id_with_case;
  begin
    Forward_Decl_Id := No_Id;
    if CD.Id_Count = Id_Table_Max then
      Fatal (IDENTIFIERS);  --  Exception is raised there.
    end if;
    CD.IdTab (No_Id).name := prefixed_Id;  --  Sentinel
    --  Follow the chain of identifiers for current Level:
    while CD.IdTab (J).name /= prefixed_Id loop
      J := CD.IdTab (J).link;
    end loop;
    if J = No_Id then
      null;  --  All good: the identifier is new at this nesting level.
    elsif
       ((K = Prozedure or K = Funktion)
        and then K = CD.IdTab (J).entity
        and then CD.IdTab (J).decl_kind = spec_unresolved)
      or else
       (K = Paquetage_Body and then CD.IdTab (J).entity = Paquetage)
    then
      --  No duplicate in those cases: J is a spec., new declaration
      --  is the corresponding body.
      Forward_Decl_Id := J;
    else
      Error
        (CD,
         err_duplicate_identifier,
         A2S (prefixed_Id)
         --  & ", previous is a " & CD.IdTab (J).entity'Image
         ,
         major);
    end if;
    --  Enter identifier in table IdTab
    CD.Id_Count            := CD.Id_Count + 1;
    CD.IdTab (CD.Id_Count) :=
      (name             => prefixed_Id,
       name_with_case   => prefixed_Id_with_case,
       link             => last_id,
       entity           => K,
       read_only        => False,
       decl_kind        => complete,
       xtyp             => Undefined,
       block_or_pkg_ref => 0,
       normal           => True,
       lev              => Level,
       adr_or_sz        => 0
      );
    --  Update start of identifier chain:
    CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx := CD.Id_Count;
    if Level = 0 then
      CD.CUD.level_0_def.Include (prefixed_Id, CD.Id_Count);
    end if;
  end Enter;

  ------------------------------------------------------------------
  -------------------------------------------------------EnterArray-

  procedure Enter_Array (
    CD        : in out Co_Defs.Compiler_Data;
    Index_STP :        Co_Defs.Exact_Subtyp
  )
  is
  begin
    if Index_STP.Discrete_First > Index_STP.Discrete_Last then
      Error (CD,
        err_illegal_array_bounds, "Low > High. NB: legal in Ada (empty array)", -- !!
        major
      );
    end if;
    if   Index_STP.Discrete_First < -HAC_Integer (XMax)
      or Index_STP.Discrete_Last  >  HAC_Integer (XMax)
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
    CD       : in out Co_Defs.Compiler_Data;
    Level    :        Defs.Nesting_level;
    Prefixed :        Boolean
  )
  is
    procedure Enter_Variable is
      dummy_id_idx : Natural;
    begin
      if CD.Sy = IDent then
        Enter (CD, Level, CD.Id, CD.Id_with_case, Variable, dummy_id_idx);
        Scanner.InSymbol (CD);
      else
        Error (CD, err_identifier_missing);
      end if;
    end Enter_Variable;
    --
    use HAT;
    need_prefix_mem : constant Boolean :=
      (not Prefixed) and then Length (CD.pkg_prefix) > 0;
    prefix_mem : VString;
  begin
    if need_prefix_mem then
      --  For instance, fields in a record must be non-prefixed.
      prefix_mem := CD.pkg_prefix;
      CD.pkg_prefix := Null_VString;
    end if;
    Enter_Variable;
    while CD.Sy = Comma loop  --  ','  in  "a, b, c : Integer;"
      Scanner.InSymbol (CD);
      Enter_Variable;
    end loop;
    if need_prefix_mem then
      CD.pkg_prefix := prefix_mem;
    end if;
  end Enter_Variables;

end HAC_Sys.Parser.Enter_Def;
