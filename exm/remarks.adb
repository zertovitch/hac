--  Remarks (warnings and notes) emitted by the HAC compiler.
--
--  Command sample:
--
--  for HAC:         hac -c     -rkruv remarks.adb
--  for GNAT:        gcc -c -gnatwkruv remarks.adb

with Interfaces;

procedure Remarks is
  use Interfaces;
  use Interfaces;       --  Note:     "use" clause already applied (...) [-rr]
  a  : Integer;         --  Note:     variable "a" is unused [-ru]
  type B is (x, y);     --  Note:     type "B" is unused [-ru]
  procedure C is null;  --  Note:     procedure "C" is unused [-ru]
  d : Integer := 123;   --  Note:     variable "d" is unused [-ru]
  e : Integer;          --  Note:     variable "e" is never read [-ru]
  f : Integer := 123;   --  Note:     variable "f" is never read [-ru]
  g : Integer := 123;   --  Note:     variable "g" is not modified, could be declared constant [-rk]
  h : Integer;          --  Warning:  variable "h" is read but never written [-rv]
  type A0 is (x0, y0);  --  Note:     "y0" is unused [-ru]
  b0 : A0 := x0;        --  Note:     variable "b0" is unused [-ru]

  procedure Missing_Read_Writes  --  Note: procedure "Missing_Read_Writes" is unused [-ru]
    (a : in     Integer;         --  Note: parameter "a" is unused [-ru]
     b : in out Integer;         --  Note: parameter "b" is unused [-ru]
     c :    out Integer;         --  Warning: parameter "c" is never written [-rv]
     d :    out Integer)         --  Warning: parameter "d" is read but never written [-rv]
  is
  begin
    if d = 5 then null; end if;
  end;

  procedure OK_Read_Writes  --  Note: procedure "OK_Read_Writes" is unused [-ru]
    (a : in     Integer;
     b : in out Integer;    --  Note: parameter "b" is unused [-ru]
     c :    out Integer)
  is
  begin
    c := a;  --  `c` is written, `a` is read, so the compiler is happy about `a` and `c`.
  end;

  function Useless return Integer is  --  Note: function "Useless" is unused [-ru]
  begin
    return 5;
  end;

begin
  e := 0;
  f := g + h;
end Remarks;
