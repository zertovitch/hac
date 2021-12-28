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

with HAC_Sys.Co_Defs, HAC_Sys.Defs, HAC_Sys.Li_Defs;

package HAC_Sys.Compiler is

  use HAC_Sys.Co_Defs, HAC_Sys.Defs;

  --  Main compilation procedure.
  --  The source code stream (CD.CUD.compiler_stream) is already
  --  available via Set_Source_Stream.
  --  If the stream stems from a file, the file must be already open and won't be closed.
  --  Compile_Main takes care of all other needed compilations around main as well.
  --
  procedure Compile_Main (
    CD                 : in out Co_Defs.Compiler_Data;
    LD                 : in out Li_Defs.Library_Data;
    main_name_hint     :        String;  --  This is used for circular unit dependency detection
    asm_dump_file_name :        String  := "";  --  Assembler output of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  );

  --  Compile unit not yet in the library.
  --  Unit's source code is compiled from a file (name: file_name)
  --  with the GNAT naming convention.
  --  Registration into the library is done after, by the librarian.
  --
  procedure Compile_Unit (
    CD                 : in out Co_Defs.Compiler_Data;
    LD                 : in out Li_Defs.Library_Data;
    upper_name         :        String;
    file_name          :        String;
    as_specification   :        Boolean;
    kind               :    out Li_Defs.Unit_Kind  --  The unit kind is discovered during parsing.
  );

  procedure Set_Message_Feedbacks (
    CD       : in out Compiler_Data;
    pipe     :        Defs.Smart_Error_Pipe;        --  Default (null): messages to Current_Error.
    progress :        Co_Defs.Compilation_Feedback  --  Default (null): messages to Current_Output.
  );

  function Unit_Compilation_Successful (CD : Compiler_Data) return Boolean;
  function Unit_Object_Code_Size (CD : Compiler_Data) return Natural;

end HAC_Sys.Compiler;
