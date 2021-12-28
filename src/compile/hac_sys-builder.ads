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
--  Builder: *the* entry point for building an executable (possibly for the p-code
--  virtual machine) from Ada sources (a main procedure and eventual depending units).

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.Li_Defs;

with HAL;

with Ada.Streams, Ada.Text_IO,
     Ada.Unchecked_Conversion;

with System;

package HAC_Sys.Builder is

  type Build_Data is record
    CD                 : Co_Defs.Compiler_Data;
    LD                 : Li_Defs.Library_Data;
    main_name_hint     : HAL.VString;  --  This is used for circular unit dependency detection
    asm_dump_file_name : HAL.VString;  --  Assembler output of compiled object code
    cmp_dump_file_name : HAL.VString;  --  Compiler dump
    listing_file_name  : HAL.VString;  --  Listing of source code with details
    var_map_file_name  : HAL.VString;  --  Output of variables (map)
  end record;

  --  Build the main procedure.
  --  The main procedure's source code stream is already
  --  available via Set_Main_Source_Stream.
  --  If the stream stems from a file, the file must be already open and won't be closed.
  --  Build_Main takes care of all other needed compilations around main as well.
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

  procedure Set_Message_Feedbacks (
    BD       : in out Build_Data;
    pipe     :        Defs.Smart_Error_Pipe;        --  Default (null): messages to Current_Error.
    progress :        Co_Defs.Compilation_Feedback  --  Default (null): messages to Current_Output.
  );

  --  Emulate GNAT's Unrestricted_Access attribute
  function Unrestricted is
    new Ada.Unchecked_Conversion (System.Address, Co_Defs.Compilation_Feedback);

  function Build_Successful (BD : Build_Data) return Boolean;
  function Object_Code_Size (BD : Build_Data) return Natural;
  function Maximum_Object_Code_Size return Natural;

  -------------------------
  --  Various utilities  --
  -------------------------

  --  Skip an eventual "shebang", e.g.: #!/usr/bin/env hac, in a text file.
  --  The Ada source begins from next line.
  --
  procedure Skip_Shebang (f : in out Ada.Text_IO.File_Type; shebang_offset : out Natural);

end HAC_Sys.Builder;
