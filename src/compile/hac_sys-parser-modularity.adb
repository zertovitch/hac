with HAC_Sys.Compiler.Library,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Modularity is

  procedure With_Clause (CD : in out Compiler_Data) is  --  10.1.2 (4)
    use Compiler.Library, Defs, Scanner, UErrors;
  begin
    InSymbol (CD);  --  Consume "with".
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, stop => True);
      end if;
      if To_String (CD.Id) = HAC_Pack_Name_Upper then
        Apply_WITH_HAC_Pack (CD);
      else
        Error (CD, err_syntax_error, "Custom units not yet supported", True);
      end if;
      InSymbol (CD);  --  Consume the identifier.
      exit when CD.Sy = Semicolon;
      Parser.Helpers.Need (CD, Comma, err_syntax_error);
    end loop;
    InSymbol (CD);  --  Consume the ';'.
  end With_Clause;

  procedure Use_Clause (CD : in out Compiler_Data; Level : PCode.Nesting_level) is  --  8.4 (2)
    use Compiler.Library, Defs, Scanner, UErrors;
  begin
    InSymbol (CD);  --  Consume "use".
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, stop => True);
      end if;
      Apply_USE_Clause (CD, Level, To_String (CD.Id_with_case));
      InSymbol (CD);  --  Consume the identifier.
      exit when CD.Sy = Semicolon;
      Parser.Helpers.Need (CD, Comma, err_syntax_error);
    end loop;
    InSymbol (CD);  --  Consume the ';'.
  end Use_Clause;

  procedure Context_Clause (CD : in out Compiler_Data) is
    use Compiler.Library, Defs;
  begin
    loop
      case CD.Sy is
        when WITH_Symbol => With_Clause (CD);
        when USE_Symbol  => Use_Clause (CD, Library_Level);
        when others => exit;
      end case;
    end loop;
  end Context_Clause;

end HAC_Sys.Parser.Modularity;
