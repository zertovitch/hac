with HAC_Pack;  use HAC_Pack;

procedure Timing is

  procedure Simple is
    T1 : constant Time := Clock;
    T2 : Time;
    D : Duration;
  begin
    Put_Line (Image (T1));
    Put_Line("Waiting 3 seconds...");
    delay 1.0 + 4.0 * 0.5;  --  HAC interpreter can be interrupted during delay.
    T2 := Clock;
    Put_Line (Image (T2));
    D := T2 - T1 + 0.0 * 1.234;
    Put_Line (Image (D));
    D := D + 1.0;
    D := D - 2.0 * 0.5;
    Put_Line (Image (D));
  end;

begin
  Simple;
end;