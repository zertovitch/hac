--  HAC <-> Native data exchange demo.
--  Native side (compile with a "full Ada" compiler like GNAT).

with Ada.Text_IO;
with HAC_Sys.Builder, HAC_Sys.PCode.Interpreter;

procedure Exchange_Native_Side is
  use Ada.Text_IO;
  use HAC_Sys.PCode.Interpreter;

  procedure Parameterless_Callback is
  begin
    Put_Line ("Native: Parameterless_Callback is speaking");
  end Parameterless_Callback;

  BD : HAC_Sys.Builder.Build_Data;
  post_mortem : Post_Mortem_Data;

  hac_program_name : String := "src/apps/exchange_hac_side.adb";

begin
  Put_Line ("Native_Side is started.");
  --
  Put_Line ("Building HAC program, " & hac_program_name);
  BD.Build_Main_from_File (hac_program_name);
  if BD.Build_Successful then
    Interpret_on_Current_IO (BD, 1, "", post_mortem);
    if Is_Exception_Raised (post_mortem.Unhandled) then
      Put_Line (Current_Error, "HAC VM: raised " & Image (post_mortem.Unhandled));
      Put_Line (Current_Error, Message (post_mortem.Unhandled));
    end if;
  end if;
end Exchange_Native_Side;
