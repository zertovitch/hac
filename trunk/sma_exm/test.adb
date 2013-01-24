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
  --
  procedure Fibo_demo(X: Integer) is -- !! Integer to be defined in HAC

    function Fib(P: Integer) return Integer is
    begin
      if P <= 2 then
        return 1;
      else
        return Fib(P-1) + Fib(P-2);
      end if;
    end Fib;

  begin
    Put("Fibonacci(");
    Put(X);
    Put(" ) = ");
    Put(Fib(X));
    New_Line;
  end Fibo_demo;

begin
  Put('A');
  c:= 'B';
  Put(c);
  new_line;
  x:= 123;
  z1.x:= Add(432,x); -- z1.x = 555
  Put(z1.x);
  New_Line;
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
  for n in 1..20 loop
    Fibo_demo(n);
  end loop;
end Test;
