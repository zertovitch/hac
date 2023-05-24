-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------
--  Native side                          --
-------------------------------------------

--  Native & master side of the demo.
--  HAC is embedded in this application.
--  Compile this program with a "full Ada" compiler like GNAT.

with Ada.Directories,
     Ada.Text_IO;

with HAC_Sys.Builder,
     HAC_Sys.PCode.Interpreter;

with Exchange_Native_Side_Pkg_Simple;

procedure Exchange_Native_Side_Simple is
  use Ada.Text_IO;
  use HAC_Sys.PCode.Interpreter;
  use Exchange_Native_Side_Pkg_Simple;

  BD : HAC_Sys.Builder.Build_Data;

  procedure Run is
    post_mortem : Post_Mortem_Data;
  begin
    Interpret_on_Current_IO (BD, 1, "", post_mortem);
    if Is_Exception_Raised (post_mortem.Unhandled) then
      Put_Line (Current_Error, "HAC VM: raised " & Image (post_mortem.Unhandled));
      Put_Line (Current_Error, Message (post_mortem.Unhandled));
    end if;
  end Run;

begin
  Register_All_Callbacks (BD);
  Ada.Directories.Set_Directory ("data_exchange_simple");
  for i in Positive loop
    New_Line;
    Put_Line
      ("Native: Run #" & i'Image &
       "   --------------------------------------------------------");
    BD.Build_Main_from_File ("exchange_hac_side_simple.adb");
    if BD.Build_Successful then
      Set_Global_VM_Variable (BD, "String from Native");
      Put_Line
        ("Native: VM variable (pre vitam) is: " &
         Get_Global_VM_Variable (BD));
      Run;
      Put_Line
        ("Native: VM variable (post mortem) is: " &
         Get_Global_VM_Variable (BD));
    end if;
    delay 3.0;
  end loop;
end Exchange_Native_Side_Simple;
