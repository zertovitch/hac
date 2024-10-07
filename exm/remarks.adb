--  Remarks (warnings and notes) emitted by the HAC compiler.
--
--  Command sample:
--
--  for HAC:         hac -c     -rkruv remarks.adb
--  for GNAT:        gcc -c -gnatwkruv remarks.adb

with Interfaces;

procedure Remarks is

  use Interfaces;
  use Interfaces;       --  Note:    "use" clause already applied (...) [-rr]
  a  : Integer;         --  Note:    variable "a" is not referenced [-ru]
  type B is (x, y);     --  Note:    type "B" is not referenced [-ru]
  procedure C is null;  --  Note:    procedure "C" is not referenced [-ru]
  d : Integer := 123;   --  Note:    variable "d" is not referenced [-ru]
                        --  Note:    variable "d" is not modified, could be declared constant [-rk]
  e : Integer;          --  Note:    variable "e" is never read [-ru]
  f : Integer := 123;   --  Note:    variable "f" is never read [-ru]
  g : Integer := 123;   --  Note:    variable "g" is not modified, could be declared constant [-rk]
  h : Integer;          --  Warning: variable "h" is read but never written [-rv]
  type A0 is (x0, y0);  --  Note:    item "y0" is not referenced [-ru]
  b0 : A0 := x0;        --  Note:    variable "b0" is not referenced [-ru]
                        --  Note:    variable "b0" is not modified, could be declared constant [-rk]

  procedure Missing_Read_Writes   --  Note:    procedure "Missing_Read_Writes" is not referenced [-ru]
    (a1 : in     Integer;         --  Note:    parameter "a1" is not referenced [-ru]
     b1 : in out Integer;         --  Note:    parameter "b1" is not referenced [-ru]
     c1 :    out Integer;         --  Warning: parameter "c1" is never written [-rv]
                                  --  Note:    parameter "c1" is not referenced [-ru]
     d1 :    out Integer;         --  Warning: parameter "d1" is read but never written [-rv]
     e1 :    out Integer)         --  `e1` is written -> compiler is happy.
  is
  begin
    if d1 = 5 then null; end if;  --  Warning: parameter "d1" is read but not written at this point [-rv]
    if e1 = 5 then null; end if;  --  Warning: parameter "e1" is read but not written at this point [-rv]
    e1 := 2;
  end Missing_Read_Writes;

  procedure OK_Read_Writes   --  Note: procedure "OK_Read_Writes" is not referenced [-ru]
    (a2 : in     Integer;
     b2 : in out Integer;    --  Note: parameter "b2" is not referenced [-ru]
     c2 :    out Integer)
  is
  begin
    c2 := a2;  --  `c2` is written, `a2` is read, so the compiler is happy about `a2` and `c2`.
  end OK_Read_Writes;

  function Useless return Integer is  --  Note: function "Useless" is not referenced [-ru]
  begin
    return 5;
  end Useless;

  --  Example appeared @
  --     https://www.reddit.com/r/ada/comments/1ezm9d6/the_variable_may_not_be_initialized/

  type Array_Of_Naturals is array (1 .. 5) of Natural;

  function Max_Array (A : Array_Of_Naturals) return Natural is
    Max : Natural;
  begin
    for I in A'Range loop
      if A (I) > Max then  --  Warning: variable "Max" is read before it is ever written [-rv]
        Max := A (I);
      end if;
    end loop;
    return Max;
  end Max_Array;

  function Max_Array_2 (A : Array_Of_Naturals) return Natural is
    Max_2 : Natural;
    Further_Iteration : Boolean := False;
  begin
    for I in A'Range loop
      if Further_Iteration then
        if A (I) > Max_2 then  --  Warning: variable "Max_2" may be read before it is ever written [-rv]
          --
          --  ^ The warning is softer than for Max: when there is a condition
          --    within a loop, we are not sure this is the first iteration.
          --    Then, we cannot be sure that Max_2 is not initialized.
          --    In this example, Max_2 is actually initialized on the first iteration.
          Max_2 := A (I);
        end if;
      else
        Max_2 := 0;
        Further_Iteration := True;
      end if;
    end loop;
    return Max_2;
  end Max_Array_2;

  procedure Tom (Condition : Boolean; J : out Integer) is
     I : Integer;
  begin
     if Condition then
        J := I;  --  Warning: variable "I" is read but not written at this point [-rv]
     end if;
  end Tom;

begin
  e := 0;
  f := g + h;  --  Warning: variable "h" is read but not written at this point [-rv]
end Remarks;
