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

with HAC_Sys.Co_Defs, HAC_Sys.Librarian;

package HAC_Sys.Compiler is

  --  Compile unit not yet in the library.
  --  Registration into the library is done elsewhere, by the Librarian.
  --
  procedure Compile_Unit
    (CD                     : in out Co_Defs.Compiler_Data;
     LD                     : in out Librarian.Library_Data;
     upper_name             :        String;
     file_name              :        String;
     as_specification       :        Boolean;
     as_main_unit           :        Boolean;
     needs_opening_a_stream :        Boolean;
     first_compilation      :        Boolean;  --  First compilation of whole build
     specification_id_index :        Natural;
     new_id_index           :    out Natural;
     unit_context           : in out Co_Defs.Id_Maps.Map;  --  in : empty for spec, spec's context for body
                                                           --  out: spec's context or body's full context.
     kind                   :    out Librarian.Unit_Kind;  --  The unit kind is discovered during parsing.
     needs_body             :    out Boolean);

  use Co_Defs;

  --  Initialize the compiler for an entire build.
  procedure Init_for_new_Build (CD : out Compiler_Data);

  procedure Set_Message_Feedbacks
    (CD           : in out Compiler_Data;
     trace_params : in     Compilation_Trace_Parameters);

  procedure Print_Tables (CD : in Compiler_Data);
  procedure Progress_Message (CD : Co_Defs.Compiler_Data; msg : String);
  procedure Dump_HAC_VM_Asm (CD : Co_Defs.Compiler_Data; file_name : String);

  function Unit_Compilation_Successful (CD : Compiler_Data) return Boolean;
  function Unit_Object_Code_Size (CD : Compiler_Data) return Natural;

end HAC_Sys.Compiler;
