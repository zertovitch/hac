--  This example contains a feature that is missing in HAC (at least v.0.01)

with HAT; use HAT;

procedure Overloading is

  function A (i : Integer) return Integer is
  begin
    return i + 1;
  end A;

  function A (r : Real) return Integer is
  begin
    return Integer (r) + 2;
  end A;

begin
  Put (A (0));    --  Should be 1
  Put (A (0.0));  --  Should be 2
end Overloading;
