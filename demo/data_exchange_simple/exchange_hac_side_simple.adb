-----------------------------------------
--  HAC <-> Native data exchange demo  --
-----------------------------------------
--  HAC side (simple version)          --
-----------------------------------------

--  This file is meant to be compiled by the HAC compiler which is
--  embedded in Exchange_Native_Side_Simple (exchange_native_side_simple.adb).

with HAT;

procedure Exchange_HAC_Side_Simple is

  use HAT;

  procedure In_Out_Callback (i : in out Integer) with Import => True;

  i : Integer := 1234;

begin

  HAT.Put_Line ("   [HAC]: before callback, variable i has value:" & i'Image);

  In_Out_Callback (i);

  HAT.Put_Line ("   [HAC]: after callback, variable i has value:" & i'Image);

end Exchange_HAC_Side_Simple;
