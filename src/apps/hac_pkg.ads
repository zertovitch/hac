--  This package contains call-backs for the
--  HAC command-line application.

with HAC_Sys.Co_Defs;
with HAT;

package HAC_Pkg is

  verbosity : Natural := 0;

  caveat       : constant String := "Caution: HAC is not a complete Ada compiler.";
  version_info : constant String :=
    "Compiler version: " & HAC_Sys.version & " dated " & HAC_Sys.reference & '.';

  HAC_margin_1 : constant String := "*******[ HAC ]*******   ";
  HAC_margin_2 : constant String := ". . . .[ HAC ]. . . .   ";
  HAC_margin_3 : constant String := "-------[ HAC ]-------   ";

  command_line_source_path, main_Ada_file_name : HAT.VString;

  procedure Compilation_Feedback (message : String);

  function Exists_Source (simple_file_name : String) return Boolean;
  procedure Open_Source (simple_file_name : String; stream : out HAC_Sys.Co_Defs.Source_Stream_Access);
  procedure Close_Source (simple_file_name : String);

end HAC_Pkg;
