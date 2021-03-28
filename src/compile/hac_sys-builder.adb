with HAC_Sys.Compiler;

package body HAC_Sys.Builder is

  procedure Build_Main (BD : in out Build_Data) is
    use HAL.VStr_Pkg;
  begin
    Compiler.Compile_Main (
      BD.Main_CD,
      BD.LD,
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
  begin
    Compiler.Set_Source_Stream (BD.Main_CD.SD, s, file_name, start_line);
  end Set_Main_Source_Stream;

  procedure Set_Error_Pipe (
    BD   : in out Build_Data;
    pipe :        Defs.Smart_error_pipe
  )
  is
  begin
    Compiler.Set_Error_Pipe (BD.Main_CD, pipe);
    --  ^ NB: Further unit compilations should propagate this.
  end Set_Error_Pipe;

  function Build_Successful (BD : Build_Data) return Boolean is
  begin
    return Compiler.Unit_Compilation_Successful (BD.Main_CD);
    --  !!  ... plus other compilations, plus link
  end Build_Successful;

  function Object_Code_Size (BD : Build_Data) return Natural is
  begin
    return Compiler.Unit_Object_Code_Size (BD.Main_CD);
    --  Whatever the build mode, the entire object code lands into Main_CD's object code.
  end Object_Code_Size;

  function Maximum_Object_Code_Size return Natural is
  begin
    return Defs.CDMax;
  end Maximum_Object_Code_Size;

end HAC_Sys.Builder;
