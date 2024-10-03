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
                        --  Note:     variable "d" is not modified, could be declared constant [-rk]
  e : Integer;          --  Note:     variable "e" is never read [-ru]
  f : Integer := 123;   --  Note:     variable "f" is never read [-ru]
  g : Integer := 123;   --  Note:     variable "g" is not modified, could be declared constant [-rk]
  h : Integer;          --  Warning:  parameter "h" is never written, but possibly read
  type A0 is (x0, y0);  --  Note:     "y0" is unused [-ru]
  b0 : A0 := x0;        --  Note:     variable "b0" is unused [-ru]
                        --  Note:     variable "b0" is not modified, could be declared constant [-rk]

  procedure Missing_Read_Writes  --  Note: procedure "Missing_Read_Writes" is unused [-ru]
    (a : in     Integer;         --  Note: parameter "a" is unused [-ru]
     b : in out Integer;         --  Note: parameter "b" is unused [-ru]
     c :    out Integer;         --  Warning: parameter "c" is never written
     d :    out Integer;         --  Warning: parameter "d" is never written, but possibly read
     e :    out Integer)         --  `e` is written -> compiler is happy.
  is
  begin
    if d = 5 then null; end if;  --  Warning: parameter "d" is read but not written at this point [-rv]
    if e = 5 then null; end if;  --  Warning: parameter "e" is read but not written at this point [-rv]
    e := 2;
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

  --  Example appeared @
  --     https://www.reddit.com/r/ada/comments/1ezm9d6/the_variable_may_not_be_initialized/

  type Array_Of_Naturals is array (1 .. 5) of Natural;

  function Max_Array (A : Array_Of_Naturals) return Natural is
     Max : Natural;
  begin
     for I in A'Range loop
        if A (I) > Max then  --  Warning: variable "Max" is read but not written at this point [-rv]
           Max := A (I);
        end if;
     end loop;
     return Max;
  end Max_Array;

  procedure Tom (Condition : Boolean; J : out Integer) is
     I : Integer;
  begin
     if Condition then
        J := I;
     end if;
  end Tom;
  
begin
  e := 0;
  f := g + h;  --  Warning: variable "h" is read but not written at this point [-rv]
end Remarks;
