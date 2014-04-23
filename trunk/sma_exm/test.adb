-- This is a fuzzy test for HAC, the HAC Ada compiler.

with SMALL_SP;  use SMALL_SP;

procedure Test is
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
  x_glob: Integer;
  
  procedure Do_0 is
  begin
    x_glob:= 54321;
  end Do_0;
  --
  procedure Do_1(a: Integer) is
  begin
    x_glob:= a;
  end Do_1;
  --
  procedure Do_1_in_out(a: in out Integer) is
  begin
    a:= 777;
  end Do_1_in_out;
  --
  -- type My_String is array(1..5) of Character;
  s: String(1..5);
  -- ms: My_String;
  --
  procedure My_Put(s: String) is -- ; first, last: Integer
    -- !! need array attributes !!
  begin
    --for i in first .. last loop
	  null; -- Put(s(i));
	--end loop;
  end;
  --
  
  -----------------------------------------------
  -- Recursive Fibonacci numbers demonstration --
  -----------------------------------------------
  
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
    Put("  Fibonacci(");
    Put(X);
    Put(" ) = ");
    Put(Fib(X));
    New_Line;
  end Fibo_demo;

  Twenty: constant:= 20;
  Ten_point_one: constant := 10.1;
  c: Character;
  ABCDEFGHIJKLMNOPQRSTUVWXYZ: Character; -- Testing a long identifier
begin
  Put('A');
  c:= 'B';
  Put(c);
  c:= 'x'; --- !! ''' doesn't work !!
  --
  s:= "hello";
  -- My_Put(s, 1 , 5);
  new_line;
  Put(Twenty); Put_Line(" ... should be: 20");
  Put(Ten_point_one); Put_Line(" ... should be: 1.01000E+01");
  x_glob:= 123;
  z1.x:= Add(432, x_glob); -- then: z1.x = 555
  Put(z1.x);
  Put_Line(" ... should be: 555");
  z1.y:= 1.8;
  Put(z1.y); Put_Line(" ... should be: 1.80000E+00");
  Do_0;
  Do_1(123);
  Do_1_in_out(x_glob);
  case x_glob is
    when 123 =>
      My_Put("123 (wrong)");
    when 456 =>
      My_Put("456 (wrong)");
    when 777 =>
      Put("777 (correct)");
    when others =>
      Put(x_glob);
      Put(" (wrong)");
  end case;
  New_Line;
  Put("Bla bla");
  Put("");
  Put_Line(" and more bla bla!");
  for gagl in 1..20 loop
    case gagl is
      when 4 =>
        Put_Line("choice: four");
      when 2 =>
        Put_Line("choice: two");
      -- when 2 => null; -- duplicate, is rejected
      when others =>
        Put("choice: "); 
        Put(gagl); -- !! HAC: Put(gagl, 1) prints two integer...
        Put_Line(" (others)");
    end case;
    exit when gagl = 8;
  end loop;
  --
  Put_Line("Recursive Fibonacci:");
  for n in 1..22 loop
    Fibo_demo(n);
  end loop;
end Test;
