--  This example contains a feature that is missing in HAC (at least v.0.01)

with HAC_Pack; use HAC_Pack;

procedure Overloading is

  function A (i: Integer) return Integer is
  begin
    return i;
  end A;

  function A (f: Float) return Integer is
  begin
    return Integer (f);
  end A;

begin
  Put (A (0));    --  Should be 1
  Put (A (0.0));  --  Should be 2
end;
