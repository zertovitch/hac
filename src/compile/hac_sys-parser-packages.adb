with HAC_Sys.Errors,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Tasking,
     HAC_Sys.Parser.Type_Def,
     HAC_Sys.Scanner;

with HAL;

package body HAC_Sys.Parser.Packages is

  ---------------------------
  --  Package_Declaration  --
  ---------------------------

  procedure Package_Declaration (
    CD                   : in out Co_Defs.Compiler_Data;
    FSys                 :        Defs.Symset;
    subprogram_level     :        Defs.Nesting_level;
    needs_body           :    out Boolean
  )
  is
    use Co_Defs, Defs, Errors, HAL, Helpers;
    use type HAC_Integer;
    package_name           : constant Alfa := CD.Id;
    package_name_with_case : constant Alfa := CD.Id_with_case;
    package_id_index       : constant Natural := CD.Id_Count;
    previous_pkg_prefix    : constant HAL.VString := CD.pkg_prefix;
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
    CD.Packages_Table (current_pkg_table_index).last_public_declaration  := 0;
    CD.Packages_Table (current_pkg_table_index).last_private_declaration := 0;
    --
    Scanner.InSymbol (CD);  --  Absorb the identifier symbol.
    Need (CD, IS_Symbol, err_IS_missing);
    --  Set new prefix, support also eventual subpackages:
    CD.pkg_prefix := CD.pkg_prefix & To_String (package_name) & '.';
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
          Error
            (CD,
             err_not_yet_implemented,
             "Variables and constants not yet implemented for packages",
             major);
          --  Const_Var.Var_Declaration (CD, FSys, block_data);
        when TYPE_Symbol |
             SUBTYPE_Symbol =>
          Type_Def.Type_Declaration (CD, subprogram_level, FSys + END_Symbol);
        when TASK_Symbol =>
          Tasking.Task_Declaration (CD, FSys, subprogram_level);
        when USE_Symbol =>
          Use_Clause (CD, subprogram_level);
        when PROCEDURE_Symbol | FUNCTION_Symbol =>
          Subprogram_Declaration_or_Body (CD, FSys, subprogram_level, subprogram_kind);
          if subprogram_kind = complete then
            Error
              (CD, err_syntax_error,
               ": subprogram body not allowed in package specification", major);
          end if;
          if subprogram_level = 0 then
            Scanner.InSymbol (CD);  --  Consume ';' symbol after END [Subprogram_Id].
          end if;
          needs_body := True;
        when PACKAGE_Symbol =>
          --  Subpackage:
          Scanner.InSymbol (CD);
          if CD.Sy /= IDent then
            Error (CD, err_identifier_missing, severity => major);
          end if;
          Enter_Def.Enter
            (CD,
             subprogram_level,
             To_Alfa (To_String (CD.pkg_prefix) & To_String (CD.Id)),
             To_Alfa (To_String (CD.pkg_prefix) & To_String (CD.Id_with_case)),
             Paquetage,
             dummy_forward);
          Package_Declaration (CD, FSys, subprogram_level, subpkg_needs_body);
          Need_Semicolon_after_Declaration (CD, FSys);
          needs_body := needs_body or subpkg_needs_body;
        when PRIVATE_Symbol =>
          Scanner.InSymbol (CD);
          Mark_Last_Declaration;
          if in_private then
            Error (CD, err_syntax_error, ": only one private part allowed per package");
          end if;
          in_private := True;
        when others => null;
      end case;
      exit when CD.Sy = END_Symbol;
    end loop;
    Mark_Last_Declaration;
    Scanner.InSymbol (CD);  --  Absorb END symbol
    if CD.Sy = IDent then
      --  !! For supporting child package names ("x.y.z"), reuse/share Check_ident_after_END
      if CD.Id /= package_name then
        Error
          (CD, err_incorrect_name_after_END,
           hint => To_String (package_name_with_case),
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
    CD                   : in out Co_Defs.Compiler_Data;
    FSys                 :        Defs.Symset;
    subprogram_level     :        Defs.Nesting_level
  )
  is
  begin
    pragma Compile_Time_Warning
      (Standard.True, "Package_Body unimplemented");
    raise Program_Error with "Unimplemented procedure Package_Body";
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
      Apply_USE_Clause (CD, Level, Helpers.Locate_Identifier (CD, To_Alfa (CD.Id), Level));
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
    Pkg_UName : constant String := To_String (CD.IdTab (Pkg_Idx).name);
    Id_Alias, dummy_id_idx : Natural;
    pkg_table_index : Positive;
  begin
    pragma Assert (Pkg_Idx /= No_Id);
    if CD.IdTab (Pkg_Idx).entity /= Paquetage then
      Error (CD, err_syntax_error, ": package name expected", major);
    end if;
    --  The package specification's definitions begins immediately after the
    --  package's identifier.
    --  E.g. HAL: PAQUETAGE; HAL.File_Type: TYPEMARK; ...
    --
    pkg_table_index := CD.IdTab (Pkg_Idx).block_pkg_ref;
    pragma Assert (Pkg_Idx + 1 = CD.Packages_Table (pkg_table_index).first_public_declaration);
    --
    for i in CD.Packages_Table (pkg_table_index).first_public_declaration ..
             CD.Packages_Table (pkg_table_index).last_public_declaration
    loop
      declare
        Full_UName : constant String := To_String (CD.IdTab (i).name);
        Full_Name  : String (Full_UName'Range);
        Start : Positive;
      begin
        --  We have spotted an item with the correct prefix.
        --  E.g. "STANDARD.FALSE" has the matching prefix "STANDARD.",
        --  or we have the item "ADA.STRINGS.FIXED.INDEX" and the prefix "ADA.STRINGS.FIXED.".
        Start := Full_UName'First + Pkg_UName'Length + 1;
        Full_Name := To_String (CD.IdTab (i).name_with_case);
        declare
          Short_Id_str : constant String := Full_UName (Start .. Full_UName'Last);
          Short_Id     : constant Alfa := To_Alfa (Short_Id_str);  --  Id as visible after USE.
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
              (CD, Level,
               Short_Id,
               To_Alfa (Full_Name (Start .. Full_Name'Last)),
               Alias,
               dummy_id_idx);
            CD.IdTab (CD.Id_Count).adr_or_sz := i;  --  i = Aliased entity's index.
          else
            --  Here we have found an identical and
            --  visible identifier at the same level.
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

  procedure Feed_Packages_Table (CD : in out Co_Defs.Compiler_Data) is
  begin
    CD.Packages_Count := CD.Packages_Count + 1;
    CD.IdTab (CD.Id_Count).block_pkg_ref := CD.Packages_Count;
    CD.Packages_Table (CD.Packages_Count).first_public_declaration := CD.Id_Count + 1;
  end Feed_Packages_Table;

end HAC_Sys.Parser.Packages;
