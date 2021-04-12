with HAC_Sys.Compiler,
     HAC_Sys.Librarian;

with Ada.Characters.Handling;

package body HAC_Sys.Builder is

  procedure Build_Main (BD : in out Build_Data) is
    use HAL.VStr_Pkg, Li_Defs;
  begin
    BD.LD.Library.Clear;
    BD.LD.Map.Clear;
    Librarian.Register_Unit (
      BD.LD,
      To_String (BD.main_name_hint),
      Procedure_Unit,
      In_Progress
    );
    Compiler.Compile_Main (
      BD.CD,
      BD.LD,
      To_String (BD.main_name_hint),
      To_String (BD.asm_dump_file_name),
      To_String (BD.cmp_dump_file_name),
      To_String (BD.listing_file_name),
      To_String (BD.var_map_file_name)
    );
  end Build_Main;

  procedure Set_Diagnostic_File_Names (
    BD                 : in out Build_Data;
    asm_dump_file_name :        String  := "";  --  Assembler output of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  )
  is
    use HAL;
  begin
    BD.asm_dump_file_name := To_VString (asm_dump_file_name);
    BD.cmp_dump_file_name := To_VString (cmp_dump_file_name);
    BD.listing_file_name  := To_VString (listing_file_name);
    BD.var_map_file_name  := To_VString (var_map_file_name);
  end Set_Diagnostic_File_Names;

  --  Set current main source stream (file, editor data, zipped file,...)
  procedure Set_Main_Source_Stream (
    BD         : in out Build_Data;
    s          : access Ada.Streams.Root_Stream_Type'Class;
    file_name  : in     String;       --  Can be a virtual name (editor title, zip entry)
    start_line : in     Natural := 0  --  We could have a shebang or other Ada sources before
  )
  is
    main_name_guess : constant String := Ada.Characters.Handling.To_Upper (file_name);
    last_slash, last_dot : Natural := 0;
  begin
    Co_Defs.Set_Source_Stream (BD.CD.CUD, s, file_name, start_line);
    --  Guess unit name from file name
    for i in main_name_guess'Range loop
      case main_name_guess (i) is
        when '.'       => last_dot := i;
        when '/' | '\' => last_slash := i;
        when others    => null;
      end case;
    end loop;
    if last_dot = 0  --  no dot at all
      or else last_dot < last_slash  --  dot only in a path
    then
      last_dot := main_name_guess'Last + 1;
    end if;
    BD.main_name_hint := HAL.To_VString (main_name_guess (last_slash + 1 .. last_dot - 1));
  end Set_Main_Source_Stream;

  procedure Set_Message_Feedbacks (
    BD       : in out Build_Data;
    pipe     :        Defs.Smart_error_pipe;
    progress :        Co_Defs.Compilation_Feedback
  )
  is
  begin
    Compiler.Set_Message_Feedbacks (BD.CD, pipe, progress);
  end Set_Message_Feedbacks;

  function Build_Successful (BD : Build_Data) return Boolean is
  begin
    return Compiler.Unit_Compilation_Successful (BD.CD);
    --  !!  ... plus other compilations, plus link
  end Build_Successful;

  function Object_Code_Size (BD : Build_Data) return Natural is
  begin
    return Compiler.Unit_Object_Code_Size (BD.CD);
    --  Whatever the build mode, the entire object code lands into Main_CD's object code.
  end Object_Code_Size;

  function Maximum_Object_Code_Size return Natural is
  begin
    return Defs.CDMax;
  end Maximum_Object_Code_Size;

  procedure Skip_Shebang (f : in out Ada.Text_IO.File_Type; shebang_offset : out Natural) is
    use Ada.Text_IO;
  begin
    shebang_offset := 0;
    if not End_Of_File (f) then
      declare
        possible_shebang : constant String := Get_Line (f);
      begin
        if possible_shebang'Length >= 2
          and then possible_shebang (possible_shebang'First .. possible_shebang'First + 1) = "#!"
        then
          shebang_offset := 1;  --  Ignore the first line, but count it.
        else
          Reset (f);
        end if;
      end;
    end if;
  end Skip_Shebang;

end HAC_Sys.Builder;
