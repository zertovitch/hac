procedure Exception_03 is
  procedure P1 is
    function F2 return Integer is
      a : array (1 .. 3) of Integer;
    begin
      a (4) := 5;       --  <-  *** Boom! *** : 4 is out-of-range
      return 0;
    end;
    i : Integer := F2;  --  <-  Trace-back should show this line
  begin
    null;
  end;
  dummy : Integer;
begin
  P1;                   --  <-  Trace-back should show this line
  dummy := 1234;
  dummy := 4321;
  P1;                   --  <-  No executed due to previously raised exception.
end;
