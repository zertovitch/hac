--  This package contains call-backs for the
--  HAC command-line application, as well as various helpers.

with HAC_Sys.Builder,
     HAC_Sys.Co_Defs,
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

  function Exists_Source (simple_file_name : String) return Boolean;
  procedure Open_Source (simple_file_name : String; stream : out HAC_Sys.Co_Defs.Source_Stream_Access);
  procedure Close_Source (simple_file_name : String);

  assembler_output_name : constant String := "asm_dump.pca";       --  PCA = PCode Assembler
  compiler_dump_name    : constant String := "compiler_dump.lst";

  compile_only : Boolean := False;

  procedure PLCE (s : String);  --  Put_Line on Current Error
  procedure NLCE;               --  New_Line on Current Error

  procedure Help (level : Positive);

  procedure Set_Target (name : String);

  procedure Failure;

  procedure Post_Build (BD : in out HAC_Sys.Builder.Build_Data);

  procedure Run (BD : in out HAC_Sys.Builder.Build_Data; arg_pos : Positive);

end HAC_Pkg;
