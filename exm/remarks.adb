--  Remarks emitted by the compiler.
--
--  Command sample:
--  hac -rkruv remarks.adb

procedure Remarks is
  --  Unused:
  a  : Integer;
  type B is (x, y);
  procedure C is null;
  d : Integer := 123;
  --  Unused (never read):
  e : Integer;
  f : Integer := 123;
  --  Could be constant:
  g : Integer := 123;
  --  Read but never written:
  h : Integer;
  --  Should issue no insult:
  type A0 is (x0, y0);
  b0 : A0 := x0;
begin
  e := 0;
  f := g + h;
end Remarks;
