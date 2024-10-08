with HAC_Sys.Scanner,
     HAC_Sys.Errors;

with HAT;

package body HAC_Sys.Parser.Enter_Def is

  use Co_Defs, Defs, Errors;
  use type HAC_Integer;

  ------------------------------------------------------------------
  ------------------------------------------------------Enter_Block-
  procedure Enter_Block
    (CD    : in out Co_Defs.Compiler_Data;
     Tptr  :        Integer)
  is
  begin
    if CD.Blocks_Count = BMax then
      Fatal (PROCEDURES);  --  Exception is raised there.
    end if;
    CD.Blocks_Count := CD.Blocks_Count + 1;
    declare
      New_B : Block_Table_Entry renames CD.Blocks_Table (CD.Blocks_Count);
    begin
      New_B.Id                 := CD.id_table (Tptr).name;
      New_B.Last_Id_Idx        := 0;
      New_B.First_Param_Id_Idx := 0;
      New_B.Last_Param_Id_Idx  := 0;
      New_B.SrcFrom            := CD.CUD.location.line;
    end;
  end Enter_Block;

  ------------------------------------------------------------------
  ------------------------------------------------------------Enter-
  procedure Enter_Simple
    (CD               : in out Co_Defs.Compiler_Data;
     Level            :        Defs.Nesting_Level;
     Id, Id_with_case :        Defs.Alfa;
     K                :        Co_Defs.Entity_Kind;
     Forward_Decl_Id  :    out Natural)
  is
    last_id : constant Index :=
      CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx;
    J : Integer := last_id;
    use HAT;
  begin
    Forward_Decl_Id := No_Id;
    if CD.Id_Count = Id_Table_Max then
      Fatal (IDENTIFIERS);  --  Exception is raised there.
    end if;
    CD.id_table (No_Id).name := Id;  --  Sentinel
    --  Follow the chain of identifiers for current Level:
    while CD.id_table (J).name /= Id loop
      J := CD.id_table (J).link;
    end loop;
    if J = No_Id then
      --  All good: the identifier is new at this nesting level,
      --  and we don't care about lower levels: name hiding is allowed.
      null;
    elsif
       ((K = prozedure or K = funktion)
        and then K = CD.id_table (J).entity
        and then CD.id_table (J).decl_kind = spec_unresolved)
      or else
       (K = paquetage_body and then CD.id_table (J).entity = paquetage)
    then
      --  No duplicate name in those cases: J is a specification,
      --  new declaration is the corresponding body.
      Forward_Decl_Id := J;
    else
      Error
        (CD,
         err_duplicate_identifier,
         A2S (Id)
         --  & ", previous is a " & CD.IdTab (J).entity'Image
         ,
         severity => major);
    end if;
    --  Enter identifier in table IdTab
    CD.Id_Count            := CD.Id_Count + 1;
    CD.id_table (CD.Id_Count) :=
      (name                  => Id,
       name_with_case        => Id_with_case,
       link                  => last_id,
       entity                => K,
       decl_kind             => complete,
       xtyp                  => undefined_subtyp,
       block_or_pkg_ref      => 0,
       normal                => True,
       lev                   => Level,
       adr_or_sz             => 0,
       is_referenced         => False,
       is_read               => no,
       is_written_after_init => no,
       is_initialized        => none,
       location              => CD.CUD.location);
    --
    CD.target.Mark_Declaration;
    --  Update start of identifier chain:
    CD.Blocks_Table (CD.Display (Level)).Last_Id_Idx := CD.Id_Count;
    if Level = 0 then
      CD.CUD.level_0_def.Include (Id, CD.Id_Count);
    end if;
  end Enter_Simple;

  procedure Enter_Prefixed
    (CD               : in out Co_Defs.Compiler_Data;
     Level            :        Defs.Nesting_Level;
     Id, Id_with_case :        Defs.Alfa;
     K                :        Co_Defs.Entity_Kind;
     Forward_Decl_Id  :    out Natural)
  is
    use HAT;
    prefixed_Id           : constant Alfa := CD.pkg_prefix & Id;
    prefixed_Id_with_case : constant Alfa := CD.pkg_prefix & Id_with_case;
  begin
    Enter_Simple (CD, Level, prefixed_Id, prefixed_Id_with_case, K, Forward_Decl_Id);
  end Enter_Prefixed;

  procedure Enter
    (CD               : in out Co_Defs.Compiler_Data;
     Level            :        Defs.Nesting_Level;
     prefixed         :        Boolean;
     Id, Id_with_case :        Defs.Alfa;
     K                :        Co_Defs.Entity_Kind;
     Forward_Decl_Id  :    out Natural)
  is
  begin
    if prefixed then
      Enter_Prefixed (CD, Level, Id, Id_with_case, K, Forward_Decl_Id);
    else
      Enter_Simple (CD, Level, Id, Id_with_case, K, Forward_Decl_Id);
    end if;
  end Enter;

  ------------------------------------------------------------------
  -------------------------------------------------------EnterArray-

  procedure Enter_Array
    (CD        : in out Co_Defs.Compiler_Data;
     Index_STP :        Co_Defs.Exact_Subtyp)
  is
  begin
    if Index_STP.Discrete_First > Index_STP.Discrete_Last then
      Error
        (CD,
         err_illegal_array_bounds,
         "Low > High. NB: legal in Ada (empty array)", -- !!
         severity => major);
    end if;
    if Index_STP.Discrete_First < -HAC_Integer (XMax) or else
       Index_STP.Discrete_Last  >  HAC_Integer (XMax)
    then
      Error
        (CD,
         err_illegal_array_bounds,
         "absolute value of a bound exceeds maximum possible value",
         severity => major);
    end if;
    if CD.Arrays_Count = AMax then
      Fatal (ARRAYS);  --  Exception is raised there.
    end if;
    CD.Arrays_Count := CD.Arrays_Count + 1;
    CD.Arrays_Table (CD.Arrays_Count).Index_xTyp := Index_STP;
  end Enter_Array;

  ------------------------------------------------------------------
  --------------------------------------------------Enter_Variables-

  procedure Enter_Variables
    (CD       : in out Co_Defs.Compiler_Data;
     Level    :        Defs.Nesting_Level;
     prefixed :        Boolean)
  is
    procedure Enter_Variable is
      dummy_id_idx : Natural;
    begin
      if CD.Sy = IDent then
        Enter (CD, Level, prefixed, CD.Id, CD.Id_with_case, variable_object, dummy_id_idx);
        Scanner.In_Symbol (CD);
      else
        Error (CD, err_identifier_missing);
      end if;
    end Enter_Variable;
    --
  begin
    Enter_Variable;
    while CD.Sy = Comma loop  --  ','  in  "a, b, c : Integer;"
      Scanner.In_Symbol (CD);
      Enter_Variable;
    end loop;
  end Enter_Variables;

end HAC_Sys.Parser.Enter_Def;
