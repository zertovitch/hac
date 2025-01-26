-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------

--  Callbacks on Native side.

with HAC_Sys.Interfacing;

with Ada.Text_IO;

package body Exchange_Native_Side_Pkg_Simple is
  use HAC_Sys.Interfacing;

  procedure In_Out_Callback (Data : in out HAC_Element_Array) is
    my_val : constant := 9876;
  begin

    Ada.Text_IO.Put_Line
      ("      [Native] (callback): HAC has given the following value:" &
       Integer'Image (To_Native (Data (1))));

    Data (1) := To_HAC (my_val);

    Ada.Text_IO.Put_Line
      ("      [Native] (callback): I just changed the value to..." & my_val'Image);

  end In_Out_Callback;

  procedure Register_All_Callbacks (BD : HAC_Sys.Builder.Build_Data) is
  begin
    Register (BD, In_Out_Callback'Access, "In_Out_Callback");
  end Register_All_Callbacks;

end Exchange_Native_Side_Pkg_Simple;
