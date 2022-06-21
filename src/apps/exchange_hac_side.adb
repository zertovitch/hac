--  HAC <-> Native data exchange demo.
--  This file is compiled by the HAC embedded in Native_Side (native_side.adb).

with HAL;

procedure Exchange_HAC_Side is

  procedure Parameterless_Callback
  with Import => True;

begin
  HAL.Put_Line ("HAC_Side started.");
  --
  Parameterless_Callback;
end Exchange_HAC_Side;
