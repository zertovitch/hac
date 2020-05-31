with HAC_Pack;  use HAC_Pack;

procedure Exception_03 is
  procedure P1 is
    function F2 return Integer is
      a : array (1 .. 3) of Integer;
    begin
      a (4) := 5;       --  <-  Trace-back should show this line
      return 0;
    end;
    i : Integer := F2;  --  <-  Trace-back should show this line
  begin
    null;
  end;
  dummy : Real;
begin
  P1;                   --  <-  Trace-back should show this line
  dummy := 1.234;
  dummy := 4.321;
  P1;                   --  <-  No executed due to previously raised exception.
end;
