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
  procedure Do_1_param_in_out(a: in out Integer) is
    sept: constant:= 7;
    septante: constant Integer:= 70;
  begin
    a:= septante + sept;
    declare
      sept_cents: constant Integer:= 700; -- sept * 100 too complicated !!
    begin
      a:= sept_cents + a;
      Put_Line("Block statement");
    end;
    begin
      begin
        Put_Line("Another block statement");
      end;
    end;
  end Do_1_param_in_out;
  --
  -- type My_String is array(1..5) of Character;
  -- s: String(1..5);
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

  -------------------------------------
  -- Testing multidimensional arrays --
  -------------------------------------

  procedure Test_multi_arrays is
    l1: constant:= -3;
    h1: constant:=  9;
    l2: constant:= 1;
    h2: constant:= 8;
    type T1 is array(l1..h1, l2..h2) of Integer;
    a: T1;
    b: array(l1..h1, l2..h2) of Float;
    type T2 is array(6..9) of Integer;
    type T3 is record x: Integer; y: T2; end record;
    c: array(l1..h1, l2..h2) of T3;
  begin
    for pass in 1..6 loop -- !! HAC: compiles 1..7 OK without "when 7", interpreter crashes
      for i in l1..h1 loop
        for j in l2..h2 loop
          case pass is
            when 1 => -- fill array a
              a(i,j):= i * j;
            when 2 => -- display array a
              Put(a(i,j));
              if j = h2 then
                New_Line;
              end if;
            when 3 => -- fill array b
              b(i,j):= Float(i * j);
              -- !! HAC accepts without Float(...);
              -- !! HAC issues error but compiles OK with Float(...) !!
            when 4 => -- display array b
              Put(b(i,j));
              Put(' ');
              if j = h2 then
                New_Line;
              end if;
            when 5 => -- fill array c
              c(i,j).y(7):= i * j;
            when 6 => -- display array c
              Put(c(i,j).y(7));
              if j = h2 then
                New_Line;
              end if;
          end case;
        end loop;
      end loop;
    end loop;
  end Test_multi_arrays;
  
  Twenty: constant:= 20;
  Ten_point_one: constant := 10.1;
  c: Character;
  -- ABCDEFGHIJKLMNOPQRSTUVWXYZ: Character; -- Testing a long identifier
  hs: String(1..7);
  
  procedure Show_hs is 
  begin
    -- Put(hs); -- !! issues ERR_ILLEGAL_PARAMETERS_TO_PUT:  illegal parameters to "Put"
    for i in 1..7 loop
      Put(hs(i));
    end loop;  
    New_Line;
  end Show_hs;
  
begin
  Put('A');
  c:= 'B';
  Put(c);
  c:= ''';
  Put(c);
  Put("");
  New_Line;
  --
  hs:= """Hello""";
  Show_hs;
  hs:= """Hel""lo";
  Show_hs;
  hs:= "Hel""lo""";
  Show_hs;
  hs:= "Hel""l""o";
  Show_hs;
  hs:= "Hel'l'o";
  Show_hs;
  --
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
  Do_1_param_in_out(x_glob);
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
  --
  Test_multi_arrays;
end Test;
