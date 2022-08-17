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
     HAC_Sys.Librarian;

with HAT;

with Ada.Containers.Hashed_Maps,
     Ada.Finalization,
     Ada.Streams,
     Ada.Strings.Unbounded.Hash,
     Ada.Text_IO,
     Ada.Unchecked_Conversion;

package HAC_Sys.Builder is

  type Compiler_Data_Access is access Co_Defs.Compiler_Data;

  package String_Maps is new Ada.Containers.Hashed_Maps
    (Key_Type        => HAT.VString,
     Element_Type    => HAT.VString,
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => HAT."=",
     "="             => HAT."=");

  type Build_Data is new Ada.Finalization.Limited_Controlled with record
    CD                  : Compiler_Data_Access := new Co_Defs.Compiler_Data;
    LD                  : Librarian.Library_Data;
    global_VM_variables : String_Maps.Map;
    main_name_hint      : HAT.VString;  --  This is used for circular unit dependency detection
    asm_dump_file_name  : HAT.VString;  --  Assembler output of compiled object code
    cmp_dump_file_name  : HAT.VString;  --  Compiler dump
    listing_file_name   : HAT.VString;  --  Listing of source code with details
    var_map_file_name   : HAT.VString;  --  Output of variables (map)
  end record;

  overriding procedure Finalize (BD : in out Build_Data);

  --  Build the main procedure.
  --  The main procedure's source code stream is already
  --  available via Set_Main_Source_Stream.
  --  If the stream stems from a file, the file must be already open and won't be closed.
  --  Build_Main takes care of all other needed compilations around main as well.
  --
  procedure Build_Main (BD : in out Build_Data);

  procedure Build_Main_from_File (BD : in out Build_Data; File_Name : String);

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
    BD           : in out Build_Data;
    trace_params : in     Co_Defs.Compilation_Trace_Parameters
  );

  function Build_Successful (BD : Build_Data) return Boolean;
  function Total_Compiled_Lines (BD : Build_Data) return Natural;
  function Object_Code_Size (BD : Build_Data) return Natural;
  function Folded_Instructions (BD : Build_Data) return Natural;
  function Specialized_Instructions (BD : Build_Data) return Natural;
  function Maximum_Object_Code_Size return Natural;

  -------------------------
  --  Various utilities  --
  -------------------------

  --  Skip an eventual "shebang", e.g.: #!/usr/bin/env hac, in a text file.
  --  The Ada source begins from next line.
  --
  procedure Skip_Shebang (f : in out Ada.Text_IO.File_Type; shebang_offset : out Natural);

end HAC_Sys.Builder;
