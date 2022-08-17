--  This package contains call-backs for the
--  HAC command-line application.

with HAC_Sys.Librarian;

with Ada.Directories;

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
      if path (i) = ',' or path (i) = ';' then
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

end HAC_Pkg;
