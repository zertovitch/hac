-----------------------------------------
--  HAC <-> Native data exchange demo  --
-----------------------------------------

--  This file is compiled by the HAC compiler embedded
--  in Exchange_Native_Side (exchange_native_side.adb).

with HAL;

procedure Exchange_HAC_Side is

  use HAL;

  procedure Demo_Parameterless is
    procedure Parameterless_Callback
    with Import => True;
  begin
    Put_Line ("   HAC: I call a parameterless callback.");
    Parameterless_Callback;
    Put_Line ("   HAC: done calling.");
  end Demo_Parameterless;

begin
  Put_Line ("Exchange_HAC_Side is started.");
  --
  Demo_Parameterless;
end Exchange_HAC_Side;
