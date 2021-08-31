--  This is a minimalistic version of HAC, for showing the minimum code required
--  to run HAC on a given Ada source file and display outputs on Standard_Output.
--
--  See HAC (hac.adb) for the full version of the command-line tool.

with HAC_Sys.Builder, HAC_Sys.PCode.Interpreter;
with Ada.Command_Line, Ada.Streams.Stream_IO, Ada.Text_IO;

procedure HAC_Mini is

  use Ada.Text_IO;

  procedure Compile_and_interpret_file (Ada_file_name : String) is
    use HAC_Sys.Builder, HAC_Sys.PCode.Interpreter;
    use Ada.Streams.Stream_IO;
    --
    f : Ada.Streams.Stream_IO.File_Type;
    BD : Build_Data;
    post_mortem : Post_Mortem_Data;
  begin
    Open (f, In_File, Ada_file_name);
    Set_Main_Source_Stream (BD, Stream (f), Ada_file_name);
    Build_Main (BD);
    Close (f);
    --
    if Build_Successful (BD) then
      Interpret_on_Current_IO (BD, 1, Ada_file_name, post_mortem);
      if Is_Exception_Raised (post_mortem.Unhandled) then
        Put_Line (Current_Error, "HAC VM: raised " & Image (post_mortem.Unhandled));
        Put_Line (Current_Error, Message (post_mortem.Unhandled));
      end if;
    end if;
  end Compile_and_interpret_file;

  use Ada.Command_Line;

begin
  if Argument_Count = 0 then
    Put_Line (Current_Error, "Usage: hac_mini main.adb");
  else
    Compile_and_interpret_file (Argument (1));
  end if;
end HAC_Mini;
