--  This is a minimalistic version of HAC, for showing the minimum code required
--  to run HAC on a given Ada source file and display outputs on Standard_Output.
--
--  See HAC (hac.adb) for the full version of the command-line tool.

with Ada.Command_Line, Ada.Text_IO;
with HAC_Sys.Builder, HAC_Sys.PCode.Interpreter;

procedure HAC_Mini is
  use Ada.Command_Line, Ada.Text_IO;
  use HAC_Sys.PCode.Interpreter;
  --
  BD : HAC_Sys.Builder.Build_Data;
  post_mortem : Post_Mortem_Data;
begin
  if Argument_Count = 0 then
    Put_Line (Current_Error, "Usage: hac_mini main.adb");
  else
    BD.Build_Main_from_File (Argument (1));
    if BD.Build_Successful then
      Interpret_on_Current_IO (BD, 1, "", post_mortem);
      if Is_Exception_Raised (post_mortem.Unhandled) then
        Put_Line (Current_Error, "HAC VM: raised " & Image (post_mortem.Unhandled));
        Put_Line (Current_Error, Message (post_mortem.Unhandled));
      end if;
    end if;
  end if;
end HAC_Mini;
