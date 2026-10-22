-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------
--  HAC side: a native call-back stops   --
--  this HAC program on a failure.      --
-------------------------------------------

--  This file is meant to be compiled by the HAC compiler which is
--  embedded in Exchange_Native_Side (exchange_native_side.adb).

with HAT;

procedure Exchange_HAC_Side_Failure is

  use HAT;

  procedure Failing_Callback (i : in Integer) with Import => True;

begin
  Put_Line ("   HAC: I call a native operation that may fail.");
  Failing_Callback (-123);
  Put_Line ("   HAC: you should never see this line!");
end Exchange_HAC_Side_Failure;
