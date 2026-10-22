-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------

--  Registration of callbacks on Native side.

with HAC_Sys.Builder;

package Exchange_Native_Side_Pkg is

  --  Registration of the call-back used by
  --  exchange_hac_side_failure.adb (see Run_Failure_Demo):
  procedure Register_Failing_Callback (BD : HAC_Sys.Builder.Build_Data);

  procedure Register_All_Callbacks (BD : HAC_Sys.Builder.Build_Data);

  procedure Set_Global_VM_Variable (BD : in out HAC_Sys.Builder.Build_Data);

  function Get_Global_VM_Variable (BD : HAC_Sys.Builder.Build_Data) return String;

end Exchange_Native_Side_Pkg;
