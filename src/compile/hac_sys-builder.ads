-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.Li_Defs;

with HAL;

with Ada.Streams;

package HAC_Sys.Builder is

  type Build_Data is record
    Main_CD            : Co_Defs.Compiler_Data;
    LD                 : Li_Defs.Library_Data;
    asm_dump_file_name : HAL.VString;  --  Assembler output of compiled object code
    cmp_dump_file_name : HAL.VString;  --  Compiler dump
    listing_file_name  : HAL.VString;  --  Listing of source code with details
    var_map_file_name  : HAL.VString;  --  Output of variables (map)
  end record;

  --  Main build procedure.
  --  Takes care of all needed compilations around main.
  --
  procedure Build_Main (BD : in out Build_Data);

  procedure Set_Diagnostic_File_Names (
    BD                 : in out Build_Data;
    asm_dump_file_name :        String  := "";  --  Assembler output of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  );

  --  Set current main source stream (file, editor data, zipped file,...)
  procedure Set_Main_Source_Stream (
    BD         : in out Build_Data;
    s          : access Ada.Streams.Root_Stream_Type'Class;
    file_name  : in     String;       --  Can be a virtual name (editor title, zip entry)
    start_line : in     Natural := 0  --  We could have a shebang or other Ada sources before
  );

  procedure Set_Error_Pipe (
    BD   : in out Build_Data;
    pipe :        Defs.Smart_error_pipe
  );

  function Build_Successful (BD : Build_Data) return Boolean;
  function Object_Code_Size (BD : Build_Data) return Natural;
  function Maximum_Object_Code_Size return Natural;

end HAC_Sys.Builder;
