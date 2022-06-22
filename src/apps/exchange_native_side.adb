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
    Put_Line ("      Native: Parameterless_Callback is speaking!");
  end Parameterless_Callback;

  procedure Hello_Callback (Data : in out HAC_Element_Array) is
  begin
    Put_Line ("      Native: HAC is saying: [" & To_Native (Data (1)) & ']');
  end Hello_Callback;

  procedure Ints_Callback (Data : in out HAC_Element_Array) is
    i, j, k : Integer;
  begin
    i := To_Native (Data (1));
    j := To_Native (Data (2));
    k := To_Native (Data (3));
    Put_Line
      ("      Native: HAC has sent me the numbers:" &
       Integer'Image (i) &
       Integer'Image (j) &
       Integer'Image (k));
  end Ints_Callback;

  procedure Floats_Callback (Data : in out HAC_Element_Array) is
    f, g, h : Long_Float;
  begin
    f := To_Native (Data (1));
    g := To_Native (Data (2));
    h := To_Native (Data (3));
    Put_Line
      ("      Native: HAC has sent me the numbers:" &
       Long_Float'Image (f) &
       Long_Float'Image (g) &
       Long_Float'Image (h));
  end Floats_Callback;

  procedure Hello_Callback_in_out (Data : in out HAC_Element_Array) is
  begin
    Put_Line ("      Native: HAC is saying: [" & To_Native (Data (1)) & ']');
    Data (1) := To_HAC ("No, I'm Native, you loser!");
  end Hello_Callback_in_out;

  procedure Ints_Callback_in_out (Data : in out HAC_Element_Array) is
    i : Integer;
  begin
    i := To_Native (Data (1));
    Put_Line
      ("      Native: HAC has sent me the number:" &
       Integer'Image (i) & "; I will send it back squared.");
    Data (1) := To_HAC (i ** 2);
  end Ints_Callback_in_out;

  procedure Floats_Callback_in_out (Data : in out HAC_Element_Array) is
    f : Long_Float;
  begin
    f := To_Native (Data (1));
    Put_Line
      ("      Native: HAC has sent me the number:" &
       Long_Float'Image (f) & "; I will send it back squared.");
    Data (1) := To_HAC (f ** 2);
  end Floats_Callback_in_out;

  BD : HAC_Sys.Builder.Build_Data;

  procedure Build_and_Run is
    post_mortem : Post_Mortem_Data;
    hac_program_name : constant String := "src/apps/exchange_hac_side.adb";
  begin
    Put_Line ("   Native: building a HAC program: " & hac_program_name);
    New_Line;
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
  New_Line;

  --  Register the callbacks:
  Register (BD, Parameterless_Callback'Unrestricted_Access, "Parameterless_Callback");
  Register (BD, Hello_Callback'Unrestricted_Access, "Hello_Callback");
  Register (BD, Ints_Callback'Unrestricted_Access, "Ints_Callback");
  Register (BD, Floats_Callback'Unrestricted_Access, "Floats_Callback");
  Register (BD, Hello_Callback_in_out'Unrestricted_Access, "Hello_Callback_in_out");
  Register (BD, Ints_Callback_in_out'Unrestricted_Access, "Ints_Callback_in_out");
  Register (BD, Floats_Callback_in_out'Unrestricted_Access, "Floats_Callback_in_out");

  Build_and_Run;
end Exchange_Native_Side;
