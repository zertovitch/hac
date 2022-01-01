with HAC_Sys.Parser.Helpers,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

package body HAC_Sys.Parser.Modularity is

  procedure With_Clause (  --  10.1.2 (4)
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Librarian.Library_Data
  )
  is
    use Defs, Scanner, UErrors;
  begin
    InSymbol (CD);  --  Consume "with".
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, severity => major);
      end if;
      Librarian.Apply_WITH (CD, LD, To_String (CD.Id));
      InSymbol (CD);  --  Consume the identifier.
      exit when CD.Sy = Semicolon;
      Helpers.Need (CD, Comma, err_syntax_error);
    end loop;
    InSymbol (CD);  --  Consume the ';'.
  end With_Clause;

  procedure Use_Clause (
    CD    : in out Co_Defs.Compiler_Data;
    Level :        Defs.Nesting_level
  )
  is  --  8.4 (2)
    use Librarian, Defs, Scanner, UErrors;
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

  procedure Context_Clause (
    CD : in out Co_Defs.Compiler_Data;
    LD : in out Librarian.Library_Data
  )
  is
    use Defs, Librarian;
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
