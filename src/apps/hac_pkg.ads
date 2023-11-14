--  This package contains call-backs for the
--  HAC command-line application, as well as various helpers.

with HAC_Sys.Builder,
     HAC_Sys.Files.Default,
     HAC_Sys.Targets;

with HAT;

package HAC_Pkg is

  verbosity : Natural := 0;

  version_info : constant String :=
    "Compiler version: " & HAC_Sys.version & " dated " & HAC_Sys.reference & '.';

  HAC_margin_1 : constant String := "*******[ HAC ]*******   ";
  HAC_margin_2 : constant String := "       [ HAC ]          ";
  HAC_margin_3 : constant String := "-------[ HAC ]-------   ";

  command_line_source_path, main_Ada_file_name : HAT.VString;

  target : HAC_Sys.Targets.Abstract_Machine_Reference := null;

  procedure Compilation_Feedback (message : String);

  package Path_Management is  --  Specific search path management

    type File_Catalogue is
      limited new HAC_Sys.Files.Default.File_Catalogue with null record;
      --  We enrich the default file system with searching of
      --  files through pathes.

    overriding function Exists (cat : File_Catalogue; name : String) return Boolean;

    overriding function Full_Source_Name (cat : File_Catalogue; name : String) return String;

    overriding function Is_Open (cat : File_Catalogue; name : String) return Boolean;

    overriding procedure Source_Open
      (cat    : in out File_Catalogue;
       name   : in     String;
       stream :    out HAC_Sys.Files.Root_Stream_Class_Access);

    overriding procedure Skip_Shebang
      (cat            : in out File_Catalogue;
       name           : in     String;
       shebang_offset :    out Natural);

    overriding procedure Close (cat : in out File_Catalogue; name : String);

  end Path_Management;

  compiler_dump_name : constant String := "compiler_dump.lst";

  compile_only : Boolean := False;

  procedure PLCE (s : String);  --  Put_Line on Current Error
  procedure NLCE;               --  New_Line on Current Error

  procedure Help (level : Positive);

  procedure Set_Target (name : String);

  procedure Failure;

  procedure Post_Build (BD : in out HAC_Sys.Builder.Build_Data);

  procedure Run (BD : in out HAC_Sys.Builder.Build_Data; arg_pos : Positive);

end HAC_Pkg;
