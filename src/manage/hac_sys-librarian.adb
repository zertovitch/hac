-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC_Sys.Compiler,
     HAC_Sys.Librarian.Built_In_Packages,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Packages,
     HAC_Sys.Errors;

with Ada.Characters.Handling,
     Ada.Exceptions;

package body HAC_Sys.Librarian is

  ---------------------------------------------
  --  Introduce a new unit into the library  --
  ---------------------------------------------

  procedure Register_Unit
    (LD         : in out Library_Data;
     Descriptor : in     Library_Unit)
  is
    use Librarian.Library_Name_Mapping;
    UVFN : constant HAT.VString := HAT.To_Upper (Descriptor.full_name);
    is_new : Boolean;
  begin
    is_new := LD.map.Find (UVFN) = No_Element;
    if not is_new then
      raise Program_Error with
        "Duplicate registration for unit " &
        HAT.To_String (Descriptor.full_name) &
        ". This case should be handled by Apply_WITH";
    end if;
    LD.library.Append (Descriptor);
    LD.map.Insert (UVFN, LD.library.Last_Index);
    --  HAT.PUT_LINE ("Registering: " & Full_Name);
  end Register_Unit;

  procedure Change_Unit_Details
    (LD         : in out Library_Data;
     Descriptor : in     Library_Unit)
  is
    use Library_Name_Mapping;
    UVFN : constant HAT.VString := HAT.To_Upper (Descriptor.full_name);
    c : Cursor;
    book_nr : Positive;
  begin
    c := LD.map.Find (UVFN);
    if c = No_Element then
      raise Program_Error with "Change_Unit_Status called on non-registered unit";
    end if;
    book_nr := Element (c);
    LD.library.Replace_Element (book_nr, Descriptor);
  end Change_Unit_Details;

  procedure Enter_Library_Level_Def
    (CD             : in out Co_Defs.Compiler_Data;
     Full_Ident     : in     String;  --  "Main", "Standard.False", ...
     New_Entity     : in     Co_Defs.Entity_Kind;
     Base_Type      : in     Defs.Typen;
     Size           : in     Integer;
     Discrete_First : in     Defs.HAC_Integer := Defs.HAC_Integer'First;
     Discrete_Last  : in     Defs.HAC_Integer := Defs.HAC_Integer'Last;
     is_built_in    : in     Boolean := False)
  is
    use Ada.Characters.Handling, Co_Defs, Defs;
    use type Nesting_Level;
    Alfa_Ident       : constant Alfa := S2A (Full_Ident);
    Alfa_Ident_Upper : constant Alfa := S2A (To_Upper (Full_Ident));
    last : Index := CD.Id_Count;
  begin
    CD.Id_Count := CD.Id_Count + 1;
    --  Find the last library-level definition:
    while last > 0 and then CD.IdTab (last).lev > 0 loop
      last := last - 1;
    end loop;
    CD.IdTab (CD.Id_Count) :=
      (name             => Alfa_Ident_Upper,
       name_with_case   => Alfa_Ident,
       link             => last,
       entity           => New_Entity,
       decl_kind        => complete,
       xtyp             => (TYP            => Base_Type,
                            Ref            => 0,
                            Is_Range       => False,
                            Discrete_First => Discrete_First,
                            Discrete_Last  => Discrete_Last),
       block_or_pkg_ref => 0,
       normal           => True,
       lev              => 0,
       adr_or_sz        => HAC_Integer (Size),
       is_referenced    => False,
       is_read          => no,
       is_written       => no,
       is_initialized   => none,
       location         => (0, 0, 0));

    CD.target.Mark_Declaration (is_built_in);
    CD.Blocks_Table (0).Last_Id_Idx := CD.Id_Count;
    CD.CUD.level_0_def.Include (Alfa_Ident_Upper, CD.Id_Count);
  end Enter_Library_Level_Def;

  procedure Set_Source_Access
    (LD  : in out Library_Data;
     cat : in     Files.Abstract_File_Catalogue_Reference)
  is
  begin
    LD.cat := cat;
  end Set_Source_Access;

  function Find_Unit_File_Name
    (LD        : Library_Data;
     Unit_Name : String)
  return String
  is
    GNAT_prefix : constant String := GNAT_File_Naming (Unit_Name);
    spec_fn : constant String := GNAT_prefix & ".ads";
    body_fn : constant String := GNAT_prefix & ".adb";
  begin
    if LD.cat.Exists (spec_fn) then
      return spec_fn;
    elsif LD.cat.Exists (body_fn) then
      return body_fn;
    else
      return "";
    end if;
  end Find_Unit_File_Name;

  procedure Activate_Unit (CD : in out Co_Defs.Compiler_Data; Upper_Name : in String) is
    use Co_Defs, Defs;
    unit_idx : Natural;
    upper_name_alfa : constant Alfa := S2A (Upper_Name);
    use type Nesting_Level;
  begin
    --  HAT.PUT_LINE ("WITH: Activating " & Upper_Name);
    --  Activate the unit itself:
    unit_idx := Parser.Helpers.Locate_Identifier
      (CD, upper_name_alfa, Level => 0, Level_0_Filter => False);
    CD.CUD.level_0_def.Include (upper_name_alfa, unit_idx);
    --  Only packages specifications need to have their items made visible.
    if CD.IdTab (unit_idx).entity = paquetage then
      declare
        pkg_table_entry : Package_Table_Entry
          renames CD.Packages_Table (CD.IdTab (unit_idx).block_or_pkg_ref);
      begin
        for declaration_in_pkg_index in
          pkg_table_entry.first_public_declaration ..
          pkg_table_entry.last_public_declaration
        loop
          if CD.IdTab (declaration_in_pkg_index).lev = 0 then
            --  We check that the level is 0 because subprogram
            --  parameters have level 1 and of course we don't
            --  want *them* to be globally visible. An inclusion
            --  of parameters would be especially nasty because
            --  the identifiers don't have any prefix!
            CD.CUD.level_0_def.Include
              (CD.IdTab (declaration_in_pkg_index).name,
               declaration_in_pkg_index);
          end if;
        end loop;
      end;
    end if;
  end Activate_Unit;

  procedure Compile_WITHed_Unit
    (CD         : in out Co_Defs.Compiler_Data;
     LD         : in out Library_Data;
     upper_name : in     String)
  is
    fn : constant String := Find_Unit_File_Name (LD, upper_name);
    --  ^ NB: if there is a spec, priority is with the spec.
    use Co_Defs, Defs, Errors;
    as_specification, needs_body : Boolean;
    --
    unit : Library_Unit :=
      (full_name     => HAT.To_VString (upper_name),
       kind          => Package_Declaration,  --  Temporary value
       status        => In_Progress,          --  Temporary value.
       id_index      => No_Id,                --  Temporary value.
       id_body_index => No_Id,                --  Temporary value.
       spec_context  => Id_Maps.Empty_Map);
  begin
    --
    --  Add new unit name to the library catalogue
    --
    --
    Register_Unit (LD, unit);
    --
    if fn = "" then
      Error
        (CD,
         err_library_error,
         "no file found matching the name """ &
         GNAT_File_Naming (upper_name) & ".ad*""",
         severity => major);
    else
      as_specification := fn (fn'Last) = 's';
      --  ".ads" extension ? Then it's a spec.
      --
      Compiler.Compile_Unit
        (CD                     => CD,
         LD                     => LD,
         upper_name             => upper_name,
         file_name              => fn,
         as_specification       => as_specification,
         as_main_unit           => False,
         needs_opening_a_stream => True,
         first_compilation      => False,
         specification_id_index => Co_Defs.No_Id,
         new_id_index           => unit.id_index,
         unit_context           => unit.spec_context,
         kind                   => unit.kind,
         needs_body             => needs_body);
      --
      if as_specification then
        case unit.kind is
          when Subprogram_Unit =>
            unit.status := Body_Postponed;
          when Package_Declaration =>
            unit.status := (if needs_body then Body_Postponed else Spec_Only);
          when Package_Body =>
            null;  --  Not relevant (spec.)
        end case;
      else
        unit.status := Done;
      end if;
      Change_Unit_Details (LD, unit);
      --
      --  Activate unit library-level declaration for the first time.
      --  It must be visible to the WITH-ing unit.
      --
      Activate_Unit (CD, upper_name);
    end if;
  end Compile_WITHed_Unit;

  procedure Apply_WITH
    (CD         : in out Co_Defs.Compiler_Data;
     LD         : in out Library_Data;
     Upper_Name : in     String)
  is
    use Ada.Exceptions, Defs, HAT, Errors;
    UVN : constant VString := To_VString (Upper_Name);
  begin
    if LD.map.Contains (UVN) then
      if LD.library.Element (LD.map.Element (UVN)).status = In_Progress then
        --  Ouch, we are WITH-ing a unit which is being compiled.
        raise Circular_Unit_Dependency with Upper_Name;
      end if;
      --  Definition is already somewhere in CD (from the compilation
      --  of another unit), we just need to reactivate it.
      --  This situation includes the duplicate WITH case (not nice but correct).
      --  Packages Standard, Interfaces and HAT are also reactivated on
      --  second WITH (implicitly for Standard).
      Activate_Unit (CD, Upper_Name);
    elsif Upper_Name = "STANDARD" then
      Built_In_Packages.Define_and_Register_Standard (CD, LD);
    elsif Upper_Name = "INTERFACES" then
      Built_In_Packages.Define_and_Register_Interfaces (CD, LD);
    elsif Upper_Name = "ADA"
      or else (Upper_Name'Length > 3
               and then Upper_Name (Upper_Name'First .. Upper_Name'First + 3) = "ADA.")
    then
      Error
        (CD,
         err_library_error,
         "Ada package (and children) are not yet part of" &
         " HAC's predefined library. Consider the " & HAT_Name & " package.",
         severity => major);
    elsif Upper_Name = HAT_Name then
      Built_In_Packages.Define_and_Register_HAT (CD, LD);
    elsif Upper_Name in "HAC_PACK" | "HAL" then
      Error
        (CD,
         err_obsolete_hat_name,
         HAT_Name,
         Upper_Name,
         severity => major);
    else
      begin
        Compile_WITHed_Unit (CD, LD, Upper_Name);
      exception
        when E : Circular_Unit_Dependency =>
          --  Re-raise the exception but add current unit to the
          --  dependency chain.
          raise Circular_Unit_Dependency
            with Upper_Name & " -> " & Exception_Message (E);
      end;
    end if;
  end Apply_WITH;

  procedure Apply_WITH_USE_Standard
    (CD : in out Co_Defs.Compiler_Data;
     LD : in out Library_Data)
  is
  begin
    Apply_WITH (CD, LD, "STANDARD");
    Parser.Packages.Apply_USE_Clause
      (CD,
       Library_Level,
       False,
       Parser.Helpers.Locate_Identifier (CD, Defs.S2A ("STANDARD"), 0));
  end Apply_WITH_USE_Standard;

  function GNAT_File_Naming (Unit_Name : String) return String is
    result : String := Ada.Characters.Handling.To_Lower (Unit_Name);
  begin
    for c of result loop
      if c = '.' then
        c := '-';
      end if;
    end loop;
    return result;
  end GNAT_File_Naming;

  function Ada_RM_Casing (Identifier : String) return String is
    use Ada.Characters.Handling;
    copy : String := To_Lower (Identifier);
    first_letter : Boolean := True;
  begin
    for i in copy'Range loop
      if first_letter then
        copy (i) := To_Upper (copy (i));
      end if;
      first_letter := copy (i) = '_';
    end loop;
    return copy;
  end Ada_RM_Casing;

end HAC_Sys.Librarian;
