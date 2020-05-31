with HAC_Pack;  use HAC_Pack;

procedure Exception_03 is
  procedure P1 is
    function F2 return Integer is
      a : array (1 .. 3) of Integer;
    begin
      a (4) := 5;
      return 0;
    end;
    i : Integer := F2;
  begin
    null;
  end;
begin
  P1;
end;
