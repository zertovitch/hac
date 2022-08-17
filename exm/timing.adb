with HAT; use HAT;

procedure Timing is

  procedure Simple is
    T1 : constant Time := Clock;
    T2 : Time;
    D : Duration;
    Day_Secs, Day_Mins : Integer;
  begin
    Put_Line (Image (T1));
    Day_Secs := Integer (Seconds (T1));
    Day_Mins := Day_Secs / 60;
    Put_Line (
      +"More in detail... : Year = " & Year (T1) &
      ", Month = " & Month (T1) &
      ", Day = " & Day (T1) &
      ", Hour = " & Day_Mins / 60 &
      ", Minutes = " & Day_Mins mod 60 &
      ", Seconds = " & Day_Secs mod 60
      );
    Put_Line ("Waiting 3 seconds...");
    delay 1.0 + 4.0 * 0.5;  --  HAC interpreter can be interrupted during the "delay" statement.
    T2 := Clock;
    Put_Line (Image (T2));
    D := T2 - T1 + 0.0 * 1.234;
    Put_Line (Image (D));
    D := D + 1.0;
    D := D - 2.0 * 0.5;
    Put_Line (Image (D));
  end Simple;

begin
  Simple;
end Timing;
