with HAC_Sys.Librarian;

with Show_MIT_License;

with Ada.Directories,
     Ada.Text_IO;

package body HAC_Pkg is

  procedure Compilation_Feedback (message : String) is
  begin
    case verbosity is
      when 0      => null;
      when 1      => HAT.Put_Line (message);
      when others => HAT.Put_Line (HAC_margin_2 & message);
    end case;
  end Compilation_Feedback;

  function Search_File (simple_file_name, path : String) return String is
    sep_pos : Natural := path'First - 1;
    new_sep_pos : Natural;
  begin
    for i in path'Range loop
      new_sep_pos := sep_pos;
      if path (i) in ',' | ';' then
        new_sep_pos := i;
      elsif i = path'Last then
        new_sep_pos := i + 1;
      end if;
      if new_sep_pos > sep_pos then
        declare
          full_file_name : constant String :=
            path (sep_pos + 1 .. new_sep_pos - 1) & HAT.Directory_Separator & simple_file_name;
        begin
          if HAT.Exists (full_file_name) then
            return full_file_name;
          end if;
        end;
      end if;
      sep_pos := new_sep_pos;
    end loop;
    return "";
  end Search_File;

  function Search_Source_File (simple_file_name : String) return String is
    --  Search order: same as GNAT's,
    --  cf. 4.2.2 Search Paths and the Run-Time Library (RTL).
  begin
    --  1) The directory containing the source file of the main unit
    --     being compiled (the file name on the command line).
    declare
      fn : constant String :=
        Ada.Directories.Containing_Directory (HAT.To_String (main_Ada_file_name)) &
        HAT.Directory_Separator &
        simple_file_name;
    begin
      if HAT.Exists (fn) then
        return fn;
      end if;
    end;
    --  2) Each directory named by an -I switch given on the
    --     hac command line, in the order given.
    declare
      fn : constant String :=
        Search_File (simple_file_name, HAT.To_String (command_line_source_path));
    begin
      if fn /= "" then
        return fn;
      end if;
    end;
    --  3) Omitted.
    --  4) Each of the directories listed in the value of the ADA_INCLUDE_PATH environment variable.
    declare
      fn : constant String :=
        Search_File (simple_file_name, HAT.To_String (HAT.Get_Env ("ADA_INCLUDE_PATH")));
    begin
      if fn /= "" then
        return fn;
      end if;
    end;
    return "";
  end Search_Source_File;

  function Exists_Source (simple_file_name : String) return Boolean is
  begin
    return Search_Source_File (simple_file_name) /= "";
  end Exists_Source;

  procedure Open_Source (simple_file_name : String; stream : out HAC_Sys.Co_Defs.Source_Stream_Access) is
    full_file_name : constant String := Search_Source_File (simple_file_name);
  begin
    HAC_Sys.Librarian.default_open_file (full_file_name, stream);
  end Open_Source;

  procedure Close_Source (simple_file_name : String) is
    full_file_name : constant String := Search_Source_File (simple_file_name);
  begin
    HAC_Sys.Librarian.default_close_file (full_file_name);
  end Close_Source;

  procedure PLCE (s : String) is
    use Ada.Text_IO;
  begin
    Put_Line (Current_Error, s);
  end PLCE;

  procedure NLCE is
    use Ada.Text_IO;
  begin
    New_Line (Current_Error);
  end NLCE;

  procedure Help (level : Positive) is
    use Ada.Text_IO;
  begin
    PLCE ("HAC: command-line build and execution tool for HAC (HAC Ada Compiler)");
    PLCE (version_info);
    PLCE ("Main URL: "           & HAC_Sys.web);
    PLCE ("  Sources, site #1: " & HAC_Sys.web2);
    PLCE ("  Sources, site #2: " & HAC_Sys.web3);
    PLCE ("  Alire Crate: "      & HAC_Sys.web4);
    NLCE;
    PLCE ("Usage: hac [options] main.adb [command-line parameters for main]");
    NLCE;
    PLCE ("Options: -a     : assembler output in " & assembler_output_name);
    PLCE ("         -c     : compile only");
    PLCE ("         -d     : dump compiler information in " & compiler_dump_name);
    PLCE ("         -h, h1 : this help");
    PLCE ("         -h2    : show more help about options");
    PLCE ("         -I     : specify source files search path (hac -h2 for details)");
    PLCE ("         -v, v1 : verbose");
    PLCE ("         -v2    : very verbose");
    PLCE ("         -wx    : enable / disable kinds of warnings (hac -h2 for details)");
    NLCE;
    PLCE ("Note: HAC (this command-line tool) accepts source files with shebang's,");
    PLCE ("      for instance:   #!/usr/bin/env hac     or     #!/usr/bin/hac");
    Show_MIT_License (Current_Error, "hac_sys.ads");
    if level > 1 then
      PLCE ("Extended help for HAC (command: hac -h2)");
      PLCE ("----------------------------------------");
      NLCE;
      PLCE ("Option -I : specify source files search path");
      NLCE;
      PLCE ("  The search path is a list of directories separated by commas (,) or semicolons (;).");
      PLCE ("  HAC searches Ada source files in the following order:");
      PLCE ("    1) The directory containing the source file of the main unit");
      PLCE ("         being compiled (the file name on the command line).");
      PLCE ("    2) Each directory named by an -I switch given on the");
      PLCE ("         hac command line, in the order given.");
      PLCE ("    3) Each of the directories listed in the value of the ADA_INCLUDE_PATH");
      PLCE ("         environment variable.");
      NLCE;
      PLCE ("Option -wx : enable warnings of kind x");
      PLCE ("       -wX : disable warnings of kind x");
      PLCE ("             x =");
      PLCE ("                 r   warnings for redundant constructs");
      NLCE;
    end if;
    Ada.Text_IO.Put ("Press Return");
    Ada.Text_IO.Skip_Line;
  end Help;

end HAC_Pkg;
