--  Remarks (warnings and notes) emitted by the HAC compiler.
--
--  Command sample:
--
--      hac        -rkruv remarks.adb
--      gcc -c -gnatwkruv remarks.adb

with Interfaces;

procedure Remarks is
  use Interfaces;
  use Interfaces;       --  Note:     Redundant [-rr]
  a  : Integer;         --  Note:     Unused [-ru]
  type B is (x, y);     --  Note:     Unused [-ru]
  procedure C is null;  --  Note:     Unused [-ru]
  d : Integer := 123;   --  Note:     Unused [-ru]
  e : Integer;          --  Note:     Never read [-ru]
  f : Integer := 123;   --  Note:     Never read [-ru]
  g : Integer := 123;   --  Note:     Could be constant [-rk]
  h : Integer;          --  Warning:  Read but never written [-rv]
  type A0 is (x0, y0);  --  Note:     "y0" is unused [-ru] 
  b0 : A0 := x0;        --  Note:     "b0" is unused [-ru]
begin
  e := 0;
  f := g + h;
end Remarks;
