with HAC_Sys.Librarian,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Modularity is

  procedure With_Clause (  --  10.1.2 (4)
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Li_Defs.Library_Data
  )
  is
    use Defs, Scanner, UErrors;
  begin
    InSymbol (CD);  --  Consume "with".
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, stop => True);
      end if;
      declare
        unit_name_upper : constant String := To_String (CD.Id);
      begin
        if unit_name_upper = "STANDARD" then
          null;  --  Standard is always WITH-ed by default.
        elsif unit_name_upper = HAL_Name then
          Librarian.Apply_WITH_HAL (CD);
        elsif unit_name_upper = "HAC_PACK" then
          Error (
            CD,
            err_library_error,
            "the new name of HAC_Pack is " & HAL_Name,
            True
          );
        else
          Librarian.Apply_WITH (CD, LD, unit_name_upper);
        end if;
      end;
      InSymbol (CD);  --  Consume the identifier.
      exit when CD.Sy = Semicolon;
      Helpers.Need (CD, Comma, err_syntax_error);
    end loop;
    InSymbol (CD);  --  Consume the ';'.
  end With_Clause;

  procedure Use_Clause (CD : in out Co_Defs.Compiler_Data; Level : Defs.Nesting_level) is  --  8.4 (2)
    use Librarian, Defs, Scanner, UErrors;
  begin
    InSymbol (CD);  --  Consume "use".
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, stop => True);
      end if;
      Apply_USE_Clause (CD, Level, Helpers.Locate_Identifier (CD, To_Alfa (CD.Id), Level));
      InSymbol (CD);  --  Consume the identifier.
      exit when CD.Sy = Semicolon;
      Helpers.Need (CD, Comma, err_syntax_error);
    end loop;
    InSymbol (CD);  --  Consume the ';'.
  end Use_Clause;

  procedure Context_Clause (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Li_Defs.Library_Data
  )
  is
    use Defs, Li_Defs;
  begin
    loop
      case CD.Sy is
        when WITH_Symbol => With_Clause (CD, LD);
        when USE_Symbol  => Use_Clause (CD, Library_Level);
        when others => exit;
      end case;
    end loop;
  end Context_Clause;

end HAC_Sys.Parser.Modularity;
