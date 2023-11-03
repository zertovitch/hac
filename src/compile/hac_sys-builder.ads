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
--  virtual machine) from Ada sources (a main procedure and possible depending units).

with HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.Librarian,
     HAC_Sys.Targets;

with HAT;

with Ada.Containers.Hashed_Maps,
     Ada.Finalization,
     Ada.Streams,
     Ada.Strings.Unbounded.Hash,
     Ada.Text_IO;

package HAC_Sys.Builder is

  package String_Maps is new Ada.Containers.Hashed_Maps
    (Key_Type        => HAT.VString,
     Element_Type    => HAT.VString,
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => HAT."=",
     "="             => HAT."=");

  type Build_Data is new Ada.Finalization.Limited_Controlled with record
    CD                  : Co_Defs.Compiler_Data_Access := new Co_Defs.Compiler_Data;
    LD                  : Librarian.Library_Data;
    global_VM_variables : String_Maps.Map;
    global_remarks      : Defs.Remark_Set := Defs.default_remarks;
    main_name_hint      : HAT.VString;       --  This is used for circular unit dependency detection
    asm_dump            : Boolean := False;  --  Assembler output of compiled object code
    cmp_dump_file_name  : HAT.VString;       --  Compiler dump
    listing_file_name   : HAT.VString;       --  Listing of source code with details
    obj_map_file_name   : HAT.VString;       --  Output of variables (map)
    target              : Targets.Abstract_Machine_Reference := null;  --  Always heap-allocated!
  end record;

  overriding procedure Finalize (BD : in out Build_Data);

  type Rounds_Range is range 0 .. 1e9;
  compile_only : constant Rounds_Range := Rounds_Range'First;
  full_build   : constant Rounds_Range := Rounds_Range'Last;

  --  Build a main unit (possibly, the main procedure).
  --  The main unit's source code stream is already
  --  available via Set_Main_Source_Stream.
  --  If the stream stems from a file, the file must be already open and won't be closed.
  --
  --  Build_Main takes care of all other needed compilations around main as well,
  --  depending on the value of body_compilation_rounds_limit.
  --    body_compilation_rounds_limit = 0 -> compile the given unit only, plus the WITH-ed specs.
  --    body_compilation_rounds_limit = full_build (default) -> main procedure will be executable.
  --
  procedure Build_Main
    (BD                            : in out Build_Data;
     body_compilation_rounds_limit :        Rounds_Range := full_build);

  procedure Build_Main_from_File (BD : in out Build_Data; File_Name : String);

  procedure Set_Diagnostic_Parameters
    (BD                 : in out Build_Data;
     asm_dump           :        Boolean := False;  --  Assembler output of compiled object code
     cmp_dump_file_name :        String  := "";     --  Compiler dump
     listing_file_name  :        String  := "";     --  Listing of source code with details
     obj_map_file_name  :        String  := "");    --  Output of objects (map)

  procedure Set_Remark_Set
    (BD  : in out Build_Data;
     set : in     Defs.Remark_Set);

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

  procedure Set_Target
    (BD         : in out Build_Data;
     new_target :        Targets.Abstract_Machine_Reference);

  function Build_Successful (BD : Build_Data) return Boolean;
  function Total_Compiled_Lines (BD : Build_Data) return Natural;
  function Object_Code_Size (BD : Build_Data) return Natural;
  function Folded_Instructions (BD : Build_Data) return Natural;
  function Specialized_Instructions (BD : Build_Data) return Natural;
  function Maximum_Object_Code_Size return Natural;

  -------------------------
  --  Various utilities  --
  -------------------------

  --  Skip an possible "shebang", e.g.: #!/usr/bin/env hac, in a text file.
  --  The Ada source begins from next line.
  --
  procedure Skip_Shebang (f : in out Ada.Text_IO.File_Type; shebang_offset : out Natural);

end HAC_Sys.Builder;
