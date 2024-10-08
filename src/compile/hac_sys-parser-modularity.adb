with HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Packages,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Modularity is

  procedure With_Clause   --  10.1.2 (4)
    (CD : in out Co_Defs.Compiler_Data;
     LD : in out Librarian.Library_Data)
  is
    use Defs, Scanner, Errors;
  begin
    In_Symbol (CD);  --  Consume "with".
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, severity => major);
      end if;
      --
      --  TBD: parse '.' for child units, like Locate_Identifier_Internal
      --  with `using_parsed_Id` = True
      --
      Librarian.Apply_WITH (CD, LD, A2S (CD.Id));
      In_Symbol (CD);  --  Consume the identifier.
      exit when CD.Sy = Semicolon;
      Helpers.Need (CD, Comma, err_general_error);
    end loop;
    In_Symbol (CD);  --  Consume the ';'.
  end With_Clause;

  procedure Context_Clause
    (CD : in out Co_Defs.Compiler_Data;
     LD : in out Librarian.Library_Data)
  is
    use Defs, Librarian;
  begin
    loop
      case CD.Sy is
        when WITH_Symbol => With_Clause (CD, LD);
        when USE_Symbol  => Packages.Use_Clause (CD, Library_Level, False);
        when others => exit;
      end case;
    end loop;
  end Context_Clause;

end HAC_Sys.Parser.Modularity;
