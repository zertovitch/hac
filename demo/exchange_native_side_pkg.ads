-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------

--  Registration of callbacks on Native side.

with HAC_Sys.Builder;

package Exchange_Native_Side_Pkg is

  procedure Register_All_Callbacks (BD : HAC_Sys.Builder.Build_Data);

  procedure Set_Global (BD : in out HAC_Sys.Builder.Build_Data);

  function Get_Global (BD : HAC_Sys.Builder.Build_Data) return String;

end Exchange_Native_Side_Pkg;
