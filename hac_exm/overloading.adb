--  This example contains a feature that is missing in HAC 0.01

with HAC_Pack; use HAC_Pack;

procedure Overloading is

  function A (i: Integer) return Integer is
  begin
    return 1;
  end A;

  function A (f: Float) return Integer is
  begin
    return 2;
  end A;

begin
  Put (A (0));    --  Should be 1
  Put (A (0.0));  --  Should be 2.0
end;