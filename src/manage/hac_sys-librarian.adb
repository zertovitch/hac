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
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Errors;

with Ada.Characters.Handling,
     Ada.Exceptions;

package body HAC_Sys.Librarian is

  ---------------------------------------------
  --  Introduce a new unit into the library  --
  ---------------------------------------------

  procedure Register_Unit (
    LD        : in out Library_Data;
    Full_Name : in     String;  --  Full unit name, like "Ada.Strings.Fixed"
    Kind      : in     Unit_Kind;
    Status    : in     Compilation_Status := Done
  )
  is
    use Librarian.Library_Name_Mapping;
    VFN  : constant HAL.VString := HAL.To_VString (Full_Name);
    UVFN : constant HAL.VString := HAL.To_Upper (VFN);
    is_new : Boolean;
    New_Unit : Library_Unit;
  begin
    is_new := LD.Map.Find (UVFN) = No_Element;
    if not is_new then
      raise Program_Error with
        "Duplicate registration for unit " & Full_Name &
        ". This case should be handled by Apply_WITH";
    end if;
    New_Unit.Kind      := Kind;
    New_Unit.Full_Name := VFN;
    New_Unit.Status    := Status;
    LD.Library.Append (New_Unit);
    LD.Map.Insert (UVFN, LD.Library.Last_Index);
    --  HAL.PUT_LINE ("Registering: " & Full_Name);
  end Register_Unit;

  procedure Change_Unit_Details (
    LD         : in out Library_Data;
    Full_Name  : in     String;  --  Full unit name, like "Ada.Strings.Fixed"
    New_Kind   : in     Unit_Kind;
    New_Status : in     Compilation_Status
  )
  is
    use Library_Name_Mapping;
    VFN  : constant HAL.VString := HAL.To_VString (Full_Name);
    UVFN : constant HAL.VString := HAL.To_Upper (VFN);
    c : Cursor;
    book_nr : Positive;
    changed_book : Library_Unit;
  begin
    c := LD.Map.Find (UVFN);
    if c = No_Element then
      raise Program_Error with "Change_Unit_Status called on non-registered unit";
    end if;
    book_nr := Element (c);
    changed_book := LD.Library.Element (book_nr);
    changed_book.Status := New_Status;
    changed_book.Kind   := New_Kind;
    LD.Library.Replace_Element (book_nr, changed_book);
  end Change_Unit_Details;

  procedure Enter_Zero_Level_Def (
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
    Alfa_Ident       : constant Alfa := To_Alfa (Full_Ident);
    Alfa_Ident_Upper : constant Alfa := To_Alfa (To_Upper (Full_Ident));
    last : Index := CD.Id_Count;
  begin
    CD.Id_Count := CD.Id_Count + 1;
    --  Find the last zero-level definition:
    while last > 0 and then CD.IdTab (last).lev > 0 loop
      last := last - 1;
    end loop;
    CD.IdTab (CD.Id_Count) :=
     (
      name           => Alfa_Ident_Upper,
      name_with_case => Alfa_Ident,
      link           => last,
      entity         => New_Entity,
      read_only      => True,
      forward        => Co_Defs.body_only,
      xtyp           => (TYP            => Base_Type,
                         Ref            => 0,
                         Is_Range       => False,
                         Discrete_First => Discrete_First,
                         Discrete_Last  => Discrete_Last),
      block_ref      => 0,
      normal         => True,
      lev            => 0,
      adr_or_sz      => Size
    );
    CD.Blocks_Table (0).Last_Id_Idx := CD.Id_Count;
    CD.CUD.level_0_def.Include (Alfa_Ident_Upper);
  end Enter_Zero_Level_Def;

  procedure Apply_USE_Clause (
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    Pkg_Idx  : in     Natural  --  Index in the identifier table
  )
  is
    use Co_Defs, Defs, Parser.Enter_Def, Errors;
    use type Nesting_level;
    Pkg_UName     : constant String := To_String (CD.IdTab (Pkg_Idx).name);
    Pkg_UName_Dot : constant String := Pkg_UName & '.';
    Pkg_Initial   : constant Character := Pkg_UName (Pkg_UName'First);
    Id_Alias : Natural;
  begin
    pragma Assert (Pkg_Idx /= No_Id);
    if CD.IdTab (Pkg_Idx).entity /= Paquetage then
      Error (CD, err_syntax_error, ": package name expected", major);
    end if;
    --  The package specification's definitions begins immediately after the
    --  package's identifier.
    --  E.g. HAL: PAQUETAGE; HAL.File_Type: TYPEMARK; ...
    --
    for i in Pkg_Idx + 1 .. CD.Id_Count loop
      --  Quick exit if the first character doesn't match the package's first letter:
      exit when Initial (CD.IdTab (i).name) /= Pkg_Initial;
      declare
        Full_UName : constant String := To_String (CD.IdTab (i).name);
        Full_Name  : String (Full_UName'Range);
        Start : Positive;
      begin
        exit when
          --  We have left the public part of the package specification.
          Full_UName'Length <= Pkg_UName_Dot'Length
          or else Full_UName (Full_UName'First .. Full_UName'First - 1 + Pkg_UName_Dot'Length) /=
                   Pkg_UName_Dot;
        --  We have spotted an item with the correct prefix.
        --  E.g. "STANDARD.FALSE" has the matching prefix "STANDARD.",
        --  or we have the item "ADA.STRINGS.FIXED.INDEX" and the prefix "ADA.STRINGS.FIXED.".
        Start := Full_UName'First + Pkg_UName_Dot'Length;
        Full_Name := To_String (CD.IdTab (i).name_with_case);
        declare
          Short_Id_str : constant String := Full_UName (Start .. Full_UName'Last);
          Short_Id     : constant Alfa := To_Alfa (Short_Id_str);
        begin
          --  Check if there is already this identifier, even as a level 0 invisible definition
          --  If not, we do a "FROM Pkg IMPORT Short_Id" (as it would be in Modula-2/Python
          --  style).
          Id_Alias := Parser.Helpers.Locate_Identifier (
            CD               => CD,
            Id               => Short_Id,
            Level            => Level,
            Fail_when_No_Id  => False,
            Alias_Resolution => False,
            Level_0_Match    => False  --  We search any matching name, including at level 0.
          );
          if Id_Alias = No_Id or else CD.IdTab (Id_Alias).lev < Level then
            --  Here we enter, e.g. the "FALSE", "False" pair.
            Enter (CD, Level,
              Short_Id,
              To_Alfa (Full_Name (Start .. Full_Name'Last)),
              Alias
            );
            CD.IdTab (CD.Id_Count).adr_or_sz := i;  --  i = Aliased entity's index.
          else
            --  Here we have found an identical and
            --  visible identifier at the same level.
            if CD.IdTab (Id_Alias).entity = Alias
              and then CD.IdTab (Id_Alias).adr_or_sz = i
            then
              if Level > 0 then
                null;  --  Just a duplicate "use" (we could emit a warning for that).
              else
                if CD.CUD.level_0_def.Contains (Short_Id) then
                  null;  --  Just a duplicate "use" (we could emit a warning for that).
                else
                  --  Re-activate definition at zero level (context clause).
                  CD.CUD.level_0_def.Include (Short_Id);
                  --  HAL.PUT_LINE ("Activate USEd item: " & Short_Id_str);
                end if;
              end if;
            end if;
          end if;
        end;
      end;
    end loop;
  end Apply_USE_Clause;

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
    upper_name_alfa : constant Alfa := To_Alfa (Upper_Name);
    unit_initial : constant Character := Upper_Name (Upper_Name'First);
    unit_uname_dot : constant String := Upper_Name & '.';
  begin
    --  Activate the unit itself:
    CD.CUD.level_0_def.Include (upper_name_alfa);
    --  HAL.PUT_LINE ("WITH: Activating " & Upper_Name);
    unit_idx := Parser.Helpers.Locate_Identifier (CD, upper_name_alfa, 0);
    --  Only packages specifications need to have their items made visible.
    if CD.IdTab (unit_idx).entity /= Paquetage then
      return;
    end if;
    for Pkg_Id of CD.IdTab (unit_idx + 1 .. CD.Id_Count) loop
      --  Quick exit if the first character doesn't match the unit's first letter:
      exit when Initial (Pkg_Id.name) /= unit_initial;
      declare
        full_upper_name : constant String := To_String (Pkg_Id.name);
      begin
        exit when full_upper_name'Length <= unit_uname_dot'Length
          or else full_upper_name (full_upper_name'First .. full_upper_name'First - 1 + unit_uname_dot'Length) /=
                   unit_uname_dot;
        --  We have a Pkg.Item to activate
        CD.CUD.level_0_def.Include (Pkg_Id.name);
      end;
    end loop;
  end Activate_Unit;

  procedure Apply_Custom_WITH (
    CD         : in out Co_Defs.Compiler_Data;
    LD         : in out Library_Data;
    Upper_Name : in     String
  )
  is
    fn : constant String := Find_Unit_File_Name (Upper_Name);
    use Defs, Errors;
    kind : Unit_Kind := Package_Unit;
    --  ^ Temporary value, file may contain another kind of unit.
  begin
    --
    --  Add new unit name to the library catalogue
    --
    Register_Unit (LD, Upper_Name, kind, Status => In_Progress);
    --
    if fn = "" then
      Error (
        CD,
        err_library_error,
        ": no file found matching the name " & GNAT_Naming (Upper_Name) & ".ad*",
        major
      );
    else
      Compiler.Compile_Unit (CD, LD, Upper_Name, fn, fn (fn'Last) = 's', kind);
    end if;
    --
    Change_Unit_Details (LD, Upper_Name, kind, New_Status => Done);
    --
    --  Activate unit 0-level declaration for the first time.
    --  It must be visible to the WITH-ing unit.
    --
    Activate_Unit (CD, Upper_Name);
  end Apply_Custom_WITH;

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
      if LD.Library.Element (LD.Map.Element (UVN)).Status = In_Progress then
        raise Circular_Unit_Dependency with Upper_Name;
      end if;
      Activate_Unit (CD, Upper_Name);
    elsif Upper_Name = "STANDARD" then
      Built_In_Packages.Define_and_Register_Standard (CD, LD);
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
        Apply_Custom_WITH (CD, LD, Upper_Name);
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
    Apply_USE_Clause (
      CD, Library_Level,
      Parser.Helpers.Locate_Identifier (CD, Defs.To_Alfa ("STANDARD"), 0)
    );
  end Apply_WITH_USE_Standard;

end HAC_Sys.Librarian;
