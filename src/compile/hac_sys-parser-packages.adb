with HAC_Sys.Errors,
     HAC_Sys.Parser.Const_Var,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Tasking,
     HAC_Sys.Parser.Type_Def,
     HAC_Sys.Scanner;

with HAT;

package body HAC_Sys.Parser.Packages is

  ---------------------------
  --  Package_Declaration  --
  ---------------------------

  procedure Package_Declaration (
    CD         : in out Co_Defs.Compiler_Data;
    FSys       :        Defs.Symset;
    block_data : in out Block_Data_Type;
    needs_body :    out Boolean
  )
  is
    use Co_Defs, Defs, Errors, HAT, Helpers;
    use type HAC_Integer;
    package_name           : constant Alfa    := CD.Id;
    package_name_with_case : constant Alfa    := CD.Id_with_case;
    package_id_index       : constant Natural := CD.Id_Count;
    previous_pkg_prefix    : constant VString := CD.pkg_prefix;
    subpkg_needs_body : Boolean;
    in_private : Boolean := False;
    dummy_forward : Natural;
    current_pkg_table_index : Positive;
    subprogram_kind : Declaration_Kind;
    --
    procedure Mark_Last_Declaration is
    begin
      if CD.Id_Count > package_id_index then
        if in_private then
          CD.Packages_Table (current_pkg_table_index).last_private_declaration := CD.Id_Count;
        else
          CD.Packages_Table (current_pkg_table_index).last_public_declaration := CD.Id_Count;
        end if;
      end if;
    end Mark_Last_Declaration;
    --
  begin
    Feed_Packages_Table (CD);
    --  CD.Packages_Count can be incremented further during
    --  this procedure due to subpackages, so we need to memorize it.
    current_pkg_table_index := CD.Packages_Count;
    --
    Scanner.InSymbol (CD);  --  Absorb the identifier symbol. !! We need more for child packages.
    Need (CD, IS_Symbol, err_IS_missing);
    --  Set new prefix, support also eventual subpackages:
    CD.pkg_prefix := CD.pkg_prefix & A2S (package_name) & '.';
    needs_body := False;
    loop
      Test (
        CD, Declaration_Symbol + END_Symbol + PRIVATE_Symbol,
        Empty_Symset,
        err_incorrectly_used_symbol,
        stop_on_error => True  --  Exception is raised there if there is an error.
      );
      case CD.Sy is
        when IDent =>
          if block_data.level = 0 then
            Error
              (CD,
               err_not_yet_implemented,
               "variables and constants in packages at library level",
             major);
          end if;
          Const_Var.Var_Declaration (CD, FSys, block_data);
          Mark_Last_Declaration;
        when TYPE_Symbol |
             SUBTYPE_Symbol =>
          Type_Def.Type_Declaration (CD, block_data.level, FSys + END_Symbol);
          Mark_Last_Declaration;
        when TASK_Symbol =>
          Tasking.Task_Declaration (CD, FSys, block_data.level);
          Mark_Last_Declaration;
        when USE_Symbol =>
          Use_Clause (CD, block_data.level);
        when PROCEDURE_Symbol | FUNCTION_Symbol =>
          Subprogram_Declaration_or_Body (CD, FSys, block_data.level, subprogram_kind);
          if subprogram_kind = complete then
            Error
              (CD, err_syntax_error,
               ": subprogram body not allowed in package specification",
               major);
          end if;
          if block_data.level = 0 then
            Scanner.InSymbol (CD);  --  Consume ';' symbol after END [Subprogram_Id].
          end if;
          needs_body := True;
          Mark_Last_Declaration;
        when PACKAGE_Symbol =>
          --  Subpackage:
          Scanner.InSymbol (CD);
          case CD.Sy is
            when BODY_Symbol =>
              Error
                (CD, err_syntax_error,
                 ": subpackage body not allowed in package specification",
                 major);
            when IDent =>
              null;  --  Good!
            when others =>
              Error (CD, err_identifier_missing, severity => major);
          end case;
          Enter_Def.Enter (CD, block_data.level, CD.Id, CD.Id_with_case, Paquetage, dummy_forward);
          CD.IdTab (CD.Id_Count).decl_kind := spec_resolved;
          --  Why spec_resolved ? missing bodies for eventual suprograms
          --  in that package are checked anyway.
          Package_Declaration (CD, FSys, block_data, subpkg_needs_body);
          Need_Semicolon_after_Declaration (CD, FSys);
          needs_body := needs_body or subpkg_needs_body;
          Mark_Last_Declaration;
        when PRIVATE_Symbol =>
          Scanner.InSymbol (CD);
          if in_private then
            Error (CD, err_syntax_error, ": only one private part allowed per package");
          end if;
          in_private := True;
        when others => null;
      end case;
      exit when CD.Sy = END_Symbol;
    end loop;
    Scanner.InSymbol (CD);  --  Absorb END symbol
    if CD.Sy = IDent then
      --  !! For supporting child package names ("x.y.z"), reuse/share Check_ident_after_END
      if CD.Id /= package_name then
        Error
          (CD, err_incorrect_name_after_END,
           hint => A2S (package_name_with_case),
           severity => minor
          );
      end if;
      Scanner.InSymbol (CD);  --  Absorb identifier symbol
    end if;
    --  Test semicolon but don't absorb it (we might be at the end of the stream).
    Test
       (CD, Semicolon_Set,
        Empty_Symset,
        err_incorrectly_used_symbol,
        stop_on_error => True);  --  Exception is raised there if there is an error.
    CD.pkg_prefix := previous_pkg_prefix;
  end Package_Declaration;

  --------------------
  --  Package_Body  --
  --------------------

  procedure Package_Body (
    CD         : in out Co_Defs.Compiler_Data;
    FSys       :        Defs.Symset;
    block_data : in out Block_Data_Type
  )
  is
    use Co_Defs, Defs, Errors, HAT, Helpers;
    use type HAC_Integer;
    package_name           : constant Alfa    := CD.Id;
    package_name_with_case : constant Alfa    := CD.Id_with_case;
    previous_pkg_prefix    : constant VString := CD.pkg_prefix;
    --
    last_id : constant Defs.Index := CD.Blocks_Table (CD.Display (block_data.level)).Last_Id_Idx;
    --
    subprogram_kind                    : Declaration_Kind;
    pkg_spec_index                     : Natural;
    subpkg_needs_body, subpackage_body : Boolean;
    subpkg_kind                        : Entity_Kind;
  begin
    Scanner.InSymbol (CD);  --  Absorb the identifier symbol. !! We need more for child packages.
    Need (CD, IS_Symbol, err_IS_missing);
    CD.pkg_prefix := CD.pkg_prefix & A2S (package_name) & '.';
    loop
      Test (
        CD, Declaration_Symbol + BEGIN_Symbol + END_Symbol + PRIVATE_Symbol,
        Empty_Symset,
        err_incorrectly_used_symbol,
        stop_on_error => True  --  Exception is raised there if there is an error.
      );
      case CD.Sy is
        when IDent =>
          if block_data.level = 0 then
            Error
              (CD,
               err_not_yet_implemented,
               "variables and constants in packages at library level",
             major);
          end if;
          Const_Var.Var_Declaration (CD, FSys, block_data);
        when TYPE_Symbol |
             SUBTYPE_Symbol =>
          Type_Def.Type_Declaration (CD, block_data.level, FSys + END_Symbol);
        when TASK_Symbol =>
          Tasking.Task_Declaration (CD, FSys, block_data.level);
        when USE_Symbol =>
          Use_Clause (CD, block_data.level);
        when PROCEDURE_Symbol | FUNCTION_Symbol =>
          Subprogram_Declaration_or_Body (CD, FSys, block_data.level, subprogram_kind);
          if block_data.level = 0 then
            Scanner.InSymbol (CD);  --  Consume ';' symbol after END [Subprogram_Id].
          end if;
        when PACKAGE_Symbol =>
          --  Subpackage inside a package body.
          --  Subpackage can be spec & body, or just a spec, or the body of
          --  a spec defined in the parent package...
          Scanner.InSymbol (CD);
          subpackage_body := False;
          subpkg_kind := Paquetage;
          if CD.Sy = BODY_Symbol then
            Scanner.InSymbol (CD);
            subpackage_body := True;
            subpkg_kind := Paquetage_Body;
          end if;
          if CD.Sy /= IDent then
            Error (CD, err_identifier_missing, severity => major);
          end if;
          Enter_Def.Enter (CD, block_data.level, CD.Id, CD.Id_with_case, subpkg_kind, pkg_spec_index);
          if subpackage_body then
            if pkg_spec_index = No_Id then
              Error (CD, err_syntax_error, ": missing specification for package body", major);
            end if;
            CD.IdTab (CD.Id_Count).block_or_pkg_ref := CD.IdTab (pkg_spec_index).block_or_pkg_ref;
            Package_Body (CD, FSys, block_data);
          else
            CD.IdTab (CD.Id_Count).decl_kind := spec_resolved;
            --  Why spec_resolved ? missing bodies for eventual suprograms
            --  in that package are checked anyway.
            Package_Declaration (CD, FSys, block_data, subpkg_needs_body);
          end if;
          --  !!  Do something with subpkg_needs_body ...
          Need_Semicolon_after_Declaration (CD, FSys);
        when PRIVATE_Symbol =>
          Error (CD, err_syntax_error, ": ""private"" belongs to specification");
          Scanner.InSymbol (CD);
        when others => null;
      end case;
      exit when CD.Sy = BEGIN_Symbol or CD.Sy = END_Symbol;
    end loop;
    if CD.Sy = BEGIN_Symbol then
      Error (CD, err_not_yet_implemented, "initialisation part in packages", major);
    end if;
    Scanner.InSymbol (CD);  --  Absorb END symbol
    if CD.Sy = IDent then
      --  !! For supporting child package names ("x.y.z"), reuse/share Check_ident_after_END
      if CD.Id /= package_name then
        Error
          (CD, err_incorrect_name_after_END,
           hint => A2S (package_name_with_case),
           severity => minor
          );
      end if;
      Scanner.InSymbol (CD);  --  Absorb identifier symbol
    end if;
    --  Test semicolon but don't absorb it (we might be at the end of the stream).
    Test
       (CD, Semicolon_Set,
        Empty_Symset,
        err_incorrectly_used_symbol,
        stop_on_error => True);  --  Exception is raised there if there is an error.

    CD.pkg_prefix := previous_pkg_prefix;
    --  Make body's declarations unreachable in identifier chain.
    CD.Blocks_Table (CD.Display (block_data.level)).Last_Id_Idx := last_id;
  end Package_Body;

  procedure Use_Clause (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level
  )
  is  --  8.4 (2)
    use Defs, Scanner, Errors;
  begin
    InSymbol (CD);  --  Consume "use".
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, severity => major);
      end if;
      Apply_USE_Clause (CD, Level, Helpers.Locate_Identifier (CD, CD.Id, Level));
      InSymbol (CD);  --  Consume the identifier.
      exit when CD.Sy = Semicolon;
      Helpers.Need (CD, Comma, err_syntax_error);
    end loop;
    InSymbol (CD);  --  Consume the ';'.
  end Use_Clause;

  procedure Apply_USE_Clause (
    CD       : in out Co_Defs.Compiler_Data;
    Level    : in     Defs.Nesting_level;
    Pkg_Idx  : in     Natural  --  Index in the identifier table for USEd package.
  )
  is
    use Co_Defs, Defs, Parser.Enter_Def, Errors;
    use type Nesting_level;
    Pkg_UName : constant String := A2S (CD.IdTab (Pkg_Idx).name);
    Id_Alias, dummy_id_idx : Natural;
    pkg_table_index : Positive;
  begin
    pragma Assert (Pkg_Idx > No_Id);
    if CD.IdTab (Pkg_Idx).entity /= Paquetage then
      Error (CD, err_syntax_error, ": package name expected", major);
    end if;
    --  The package specification's definitions begins immediately after the
    --  package's identifier.
    --  E.g. HAT: PAQUETAGE; HAT.File_Type: TYPEMARK; ...
    --
    pkg_table_index := CD.IdTab (Pkg_Idx).block_or_pkg_ref;
    --
    for i in CD.Packages_Table (pkg_table_index).first_public_declaration ..
             CD.Packages_Table (pkg_table_index).last_public_declaration
    loop
      declare
        Full_UName : constant String := A2S (CD.IdTab (i).name);
        Full_Name  : String (Full_UName'Range);
        Start : Positive;
      begin
        --  We have spotted an item with the correct prefix.
        --  E.g. "STANDARD.FALSE" has the matching prefix "STANDARD.",
        --  or we have the item "ADA.STRINGS.FIXED.INDEX" and the prefix "ADA.STRINGS.FIXED.".
        Start := Full_UName'First + Pkg_UName'Length + 1;
        Full_Name := A2S (CD.IdTab (i).name_with_case);
        declare
          Short_Id_str : constant String := Full_UName (Start .. Full_UName'Last);
          Short_Id     : constant Alfa := S2A (Short_Id_str);  --  Id as visible after USE.
        begin
          --  Check if there is already this identifier, even as
          --  a library level invisible definition.
          --  If not, we do a "FROM Pkg IMPORT Short_Id" (as it would be in Modula-2/Python
          --  style).
          Id_Alias := Parser.Helpers.Locate_Identifier (
            CD               => CD,
            Id               => Short_Id,
            Level            => Level,
            Fail_when_No_Id  => False,
            Alias_Resolution => False,
            Level_0_Filter   => False
            --  ^ We search any matching name, including hidden at library level.
          );
          if Id_Alias = No_Id or else CD.IdTab (Id_Alias).lev < Level then
            --  Name was not found or defined at a lower nesting level.
            --  We enter, e.g. the "FALSE", "False" pair.
            Enter
              (CD,
               Level,
               Short_Id,
               S2A (Full_Name (Start .. Full_Name'Last)),
               Alias,
               dummy_id_idx);
            CD.IdTab (CD.Id_Count).adr_or_sz := i;  --  i = Aliased entity's index.
          else
            --  Here we have found an identical and
            --  visible short identifier at the same level.
            if CD.IdTab (Id_Alias).entity = Alias
              and then CD.IdTab (Id_Alias).adr_or_sz = i
            then
              --  Here we have an identical alias (same name and points
              --  to the same definition).
              if Level > 0 then
                null;  --  Just a duplicate "use" (we could emit a warning for that).
              else
                if CD.CUD.level_0_def.Contains (Short_Id) then
                  null;  --  Just a duplicate "use" (we could emit a warning for that).
                else
                  --  Re-activate definition at zero level (context clause).
                  CD.CUD.level_0_def.Include (Short_Id, Id_Alias);
                  --  HAT.PUT_LINE ("Activate USEd item: " & Short_Id_str);
                end if;
              end if;
            end if;
          end if;
        end;
      end;
    end loop;
  end Apply_USE_Clause;

  procedure Feed_Packages_Table (CD : in out Co_Defs.Compiler_Data) is
  begin
    CD.Packages_Count := CD.Packages_Count + 1;
    CD.IdTab (CD.Id_Count).block_or_pkg_ref := CD.Packages_Count;
    declare
      p : Co_Defs.Package_Table_Entry renames CD.Packages_Table (CD.Packages_Count);
    begin
      p.first_public_declaration := CD.Id_Count + 1;
      p.last_public_declaration  := 0;
      p.last_private_declaration := 0;
    end;
  end Feed_Packages_Table;

end HAC_Sys.Parser.Packages;
