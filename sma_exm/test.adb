-- This is a fuzzy test for HAC, the HAC Ada compiler.

with SMALL_SP;  use SMALL_SP;

procedure Test is
  Twenty: constant:= 20;
  x: Integer;
  Ten_point_one: constant := 10.1;
  c: Character;
  --
  type Type1 is record
    x: Integer;
    y: Float;
  end record;
  --
  z1: Type1;
  --
  function Add(x, y: Integer) return Integer is
    value: Integer;
  begin
    value:= x + y;
    return value;
  end Add;
  --
  procedure Do_0 is
  begin
    x:= 54321;
  end Do_0;
  --
  procedure Do_1(a: Integer) is
  begin
    x:= a;
  end Do_1;
  --
  procedure Do_1_io(a: in out Integer) is
  begin
    a:= 777;
  end Do_1_io;
  --
  procedure My_Put(s: String) is
  begin
    null;
  end;
begin
  Put('A');
  c:= 'B';
  Put(c);
  new_line;
  x:= 123;
  z1.x:= Add(1,x);
  z1.y:= 1.8;
  Do_0;
  Do_1(123);
  Do_1_io(x);
  case x is
    when 123 =>
      My_Put("123");
    when 456 =>
      My_Put("456");
    when 777 =>
      Put("666+111...");
    when others =>
      null;
  end case;
  Put("Bla bla");
  Put_Line(" and more bla bla!");
end Test;
