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

  procedure Register_Unit (
    LD         : in out Library_Data;
    Descriptor : in     Library_Unit
  )
  is
    use Librarian.Library_Name_Mapping;
    UVFN : constant HAL.VString := HAL.To_Upper (Descriptor.full_name);
    is_new : Boolean;
  begin
    is_new := LD.Map.Find (UVFN) = No_Element;
    if not is_new then
      raise Program_Error with
        "Duplicate registration for unit " &
        HAL.To_String (Descriptor.full_name) &
        ". This case should be handled by Apply_WITH";
    end if;
    LD.Library.Append (Descriptor);
    LD.Map.Insert (UVFN, LD.Library.Last_Index);
    --  HAL.PUT_LINE ("Registering: " & Full_Name);
  end Register_Unit;

  procedure Change_Unit_Details (
    LD         : in out Library_Data;
    Descriptor : in     Library_Unit
  )
  is
    use Library_Name_Mapping;
    UVFN : constant HAL.VString := HAL.To_Upper (Descriptor.full_name);
    c : Cursor;
    book_nr : Positive;
  begin
    c := LD.Map.Find (UVFN);
    if c = No_Element then
      raise Program_Error with "Change_Unit_Status called on non-registered unit";
    end if;
    book_nr := Element (c);
    LD.Library.Replace_Element (book_nr, Descriptor);
  end Change_Unit_Details;

  procedure Enter_Library_Level_Def (
    CD             : in out Co_Defs.Compiler_Data;
    Full_Ident     : in     String;  --  "Main", "Standard.False", ...
    New_Entity     : in     Co_Defs.Entity_Kind;
    Base_Type      : in     Defs.Typen;
    Size           : in     Integer;
    Discrete_First : in     Defs.HAC_Integer := 0;
    Discrete_Last  : in     Defs.HAC_Integer := 0
  )
  is
    use Ada.Characters.Handling, Defs;
    use type Nesting_level;
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
     (
      name             => Alfa_Ident_Upper,
      name_with_case   => Alfa_Ident,
      link             => last,
      entity           => New_Entity,
      read_only        => True,
      decl_kind        => Co_Defs.complete,
      xtyp             => (TYP            => Base_Type,
                           Ref            => 0,
                           Is_Range       => False,
                           Discrete_First => Discrete_First,
                           Discrete_Last  => Discrete_Last),
      block_or_pkg_ref => 0,
      normal           => True,
      lev              => 0,
      adr_or_sz        => Size
    );
    CD.Blocks_Table (0).Last_Id_Idx := CD.Id_Count;
    CD.CUD.level_0_def.Include (Alfa_Ident_Upper, CD.Id_Count);
  end Enter_Library_Level_Def;

  --  GNAT_Naming returns the file name that GNAT expects for a unit
  --  with the name Unit_Name.

  function GNAT_Naming (Unit_Name : String) return String is
    result : String := Ada.Characters.Handling.To_Lower (Unit_Name);
  begin
    for c of result loop
      if c = '.' then
        c := '-';
      end if;
    end loop;
    return result;
  end GNAT_Naming;

  --  Search for "physical" file corresponding to unit name
  --  First a spec, then a body.
  --  If nothing found, return empty string.
  --
  function Find_Unit_File_Name (
    Unit_Name : String
    --  TBD:
    --  search path for single source files;
    --  search path for zipped files (.har, like .jar ...)
  )
  return String
  is
    GNAT_prefix : constant String := GNAT_Naming (Unit_Name);
    spec_fn : constant String := GNAT_prefix & ".ads";
    body_fn : constant String := GNAT_prefix & ".adb";
  begin
    if HAL.Exists (spec_fn) then
      return spec_fn;
    elsif HAL.Exists (body_fn) then
      return body_fn;
    else
      return "";
    end if;
  end Find_Unit_File_Name;

  procedure Activate_Unit (CD : in out Co_Defs.Compiler_Data; Upper_Name : in String) is
    use Co_Defs, Defs;
    unit_idx : Natural;
    upper_name_alfa : constant Alfa := S2A (Upper_Name);
  begin
    --  HAL.PUT_LINE ("WITH: Activating " & Upper_Name);
    --  Activate the unit itself:
    unit_idx := Parser.Helpers.Locate_Identifier
      (CD, upper_name_alfa, Level => 0, Level_0_Filter => False);
    CD.CUD.level_0_def.Include (upper_name_alfa, unit_idx);
    --  Only packages specifications need to have their items made visible.
    if CD.IdTab (unit_idx).entity = Paquetage then
      declare
        pkg_table_entry : Package_Table_Entry
          renames CD.Packages_Table (CD.IdTab (unit_idx).block_or_pkg_ref);
      begin
        for declaration_in_pkg_index in
          pkg_table_entry.first_public_declaration ..
          pkg_table_entry.last_public_declaration
        loop
          CD.CUD.level_0_def.Include
            (CD.IdTab (declaration_in_pkg_index).name, declaration_in_pkg_index);
        end loop;
      end;
    end if;
  end Activate_Unit;

  procedure Compile_WITHed_Unit (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Library_Data;
    Upper_Name : in     String
  )
  is
    fn : constant String := Find_Unit_File_Name (Upper_Name);
    use Defs, Errors;
    as_specification : Boolean;
    needs_body : Boolean;
    unit : Library_Unit :=
      (full_name  => HAL.To_VString (Upper_Name),
       kind       => Package_Declaration,  --  Temporary value
       status        => In_Progress,       --  Temporary value.
       id_index      => Co_Defs.No_Id,     --  Temporary value.
       id_body_index => Co_Defs.No_Id,     --  Temporary value.
       spec_context  => Co_Defs.Id_Maps.Empty_Map);
  begin
    --
    --  Add new unit name to the library catalogue
    --
    --
    Register_Unit (LD, unit);
    --
    if fn = "" then
      Error (
        CD,
        err_library_error,
        "no file found matching the name " & GNAT_Naming (Upper_Name) & ".ad*",
        major
      );
    else
      as_specification := fn (fn'Last) = 's';
      Compiler.Compile_Unit
        (CD, LD, Upper_Name, fn, as_specification,
         Co_Defs.No_Id,
         unit.id_index,
         unit.spec_context,
         unit.kind,
         needs_body);
      --
      if as_specification then
        case unit.kind is
          when Subprogram_Unit =>
            unit.status := Body_Postponed;
          when Package_Declaration =>
            if needs_body then
              unit.status := Body_Postponed;
            else
              unit.status := Spec_Only;
            end if;
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
      Activate_Unit (CD, Upper_Name);
    end if;
  end Compile_WITHed_Unit;

  procedure Apply_WITH (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Library_Data;
    Upper_Name : in     String
  )
  is
    use Ada.Exceptions, Defs, HAL, Errors;
    UVN : constant VString := To_VString (Upper_Name);
  begin
    if LD.Map.Contains (UVN) then
      --  Definition is already somewhere in CD (from the compilation
      --  of another unit), we just need to reactivate it.
      --  This situation includes the duplicate WITH case (not nice but correct).
      --  Packages Standard and HAL are also reactivated on second WITH (implicitly for Standard).
      if LD.Library.Element (LD.Map.Element (UVN)).status = In_Progress then
        raise Circular_Unit_Dependency with Upper_Name;
      end if;
      Activate_Unit (CD, Upper_Name);
    elsif Upper_Name = "STANDARD" then
      Built_In_Packages.Define_and_Register_Standard (CD, LD);
    elsif Upper_Name = "INTERFACES" then
      Built_In_Packages.Define_and_Register_Interfaces (CD, LD);
    elsif Upper_Name = HAL_Name then
      Built_In_Packages.Define_and_Register_HAL (CD, LD);
    elsif Upper_Name = "HAC_PACK" then
      Error (
        CD,
        err_library_error,
        "the new name of HAC_Pack is " & HAL_Name,
        major
      );
    else
      begin
        Compile_WITHed_Unit (CD, LD, Upper_Name);
      exception
        when E : Circular_Unit_Dependency =>
          raise Circular_Unit_Dependency with Upper_Name & " -> " & Exception_Message (E);
      end;
    end if;
  end Apply_WITH;

  procedure Apply_WITH_USE_Standard (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Library_Data
  )
  is
  begin
    Apply_WITH (CD, LD, "STANDARD");
    Parser.Packages.Apply_USE_Clause (
      CD, Library_Level,
      Parser.Helpers.Locate_Identifier (CD, Defs.S2A ("STANDARD"), 0)
    );
  end Apply_WITH_USE_Standard;

end HAC_Sys.Librarian;
