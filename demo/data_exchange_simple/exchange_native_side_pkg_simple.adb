-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------

--  Callbacks on Native side.

with HAC_Sys.Interfacing;

with Ada.Text_IO;

package body Exchange_Native_Side_Pkg_Simple is
  use HAC_Sys.Interfacing;

  procedure In_Out_Callback (Data : in out HAC_Element_Array) is
  begin

    Ada.Text_IO.Put_Line
      ("        Native (callback): HAC has given the following value:" &
       Integer'Image (To_Native (Data (1))));

    Data (1) := To_HAC (9876);

  end In_Out_Callback;

  procedure Register_All_Callbacks (BD : HAC_Sys.Builder.Build_Data) is
  begin
    Register (BD, In_Out_Callback'Access, "In_Out_Callback");
  end Register_All_Callbacks;

  procedure Set_Global_VM_Variable (BD : in out HAC_Sys.Builder.Build_Data; message : String) is
  begin
    Set_VM_Variable (BD, "Demo_Variable", message);
  end Set_Global_VM_Variable;

  function Get_Global_VM_Variable (BD : HAC_Sys.Builder.Build_Data) return String is
  begin
    return Get_VM_Variable (BD, "Demo_Variable");
  end Get_Global_VM_Variable;

end Exchange_Native_Side_Pkg_Simple;
