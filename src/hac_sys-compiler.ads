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

with HAC_Sys.Co_Defs, HAC_Sys.Defs;

with Ada.Streams;

package HAC_Sys.Compiler is

  use HAC_Sys.Co_Defs, HAC_Sys.Defs;

  --  Main compilation procedure.
  --
  procedure Compile (
    CD                 : in out Compiler_Data;
    asm_dump_file_name :        String  := "";  --  Assembler oputput of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  );

  --  Set current source stream (file, editor data, zipped file,...)
  procedure Set_Source_Stream (
    CD        : in out Compiler_Data;
    s         : access Ada.Streams.Root_Stream_Type'Class;
    file_name :        String  --  Can be a virtual name (editor title, zip entry)
  );

  function Get_Current_Source_Name (CD : Compiler_Data) return String;

  procedure Set_Error_Pipe (
    CD   : in out Compiler_Data;
    pipe :        Smart_error_pipe
  );

  function Unit_Compilation_Successful (CD : Compiler_Data) return Boolean;

end HAC_Sys.Compiler;
