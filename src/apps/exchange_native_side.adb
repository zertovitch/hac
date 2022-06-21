-----------------------------------------
--  HAC <-> Native data exchange demo  --
-----------------------------------------

--  Native side (compile with a "full Ada" compiler like GNAT).

with Ada.Text_IO;
with HAC_Sys.Builder,
     HAC_Sys.Interfacing,
     HAC_Sys.PCode.Interpreter;

procedure Exchange_Native_Side is
  use Ada.Text_IO;
  use HAC_Sys.Interfacing, HAC_Sys.PCode.Interpreter;

  procedure Parameterless_Callback (Dummy : in out HAC_Element_Array) is
  begin
    Put_Line ("   Native: Parameterless_Callback is speaking!");
  end Parameterless_Callback;

  BD : HAC_Sys.Builder.Build_Data;

  procedure Build_and_Run is
    post_mortem : Post_Mortem_Data;
    hac_program_name : constant String := "src/apps/exchange_hac_side.adb";
  begin
    Put_Line ("   Native: building a HAC program: " & hac_program_name);
    BD.Build_Main_from_File (hac_program_name);
    if BD.Build_Successful then
      Interpret_on_Current_IO (BD, 1, "", post_mortem);
      if Is_Exception_Raised (post_mortem.Unhandled) then
        Put_Line (Current_Error, "HAC VM: raised " & Image (post_mortem.Unhandled));
        Put_Line (Current_Error, Message (post_mortem.Unhandled));
      end if;
    end if;
  end Build_and_Run;

begin
  Put_Line ("Exchange_Native_Side is started.");
  Register (BD, Parameterless_Callback'Unrestricted_Access, "Parameterless_Callback");
  Build_and_Run;
end Exchange_Native_Side;
