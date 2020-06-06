--  This is a fuzzy test for HAC, the HAC Ada compiler.

with HAC_Pack;  use HAC_Pack;

procedure Test is
  --
  type Type1 is record
    x: Integer;
    y: Real;
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
    Put_Line("NOT in a block statement");
    a:= 700 + a;
    --
    --  *** Block statements temporarily disabled (details in roland_01.adb) ***
    --
    --  What_a_nice_block:
    --  declare
    --    sept_cents: constant Integer:= 700;  --  sept * 100 too complicated !!
    --  begin
    --    a:= sept_cents + a;
    --    Put_Line("Block statement");
    --  end What_a_nice_block;
    --  Put_Line("NOT in a block statement");
    --  what_a_nice_block_2:
    --  begin
    --    Put_Line("Another block statement");
    --    what_a_NICE_block_3:
    --    begin
    --      Put_Line("Yet another block statement");
    --    end what_a_NICE_block_3;
    --  end what_a_nice_block_2;
    --  Put_Line("NOT in a block statement");
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

  procedure Fibo_demo(X: Natural) is

    function Fib(P: Natural) return Positive is
    begin
      if P <= 2 then
        return 1;
      else
        return Fib(P-1) + Fib(P-2);
      end if;
    end Fib;

  begin
    Put("  Fibonacci(");
    Put(X, 2);
    Put(") = ");
    Put(Fib(X), 5);
    New_Line;
  end Fibo_demo;

  -------------------------------------
  -- Testing multidimensional arrays --
  -------------------------------------

  procedure Test_multi_arrays is
    l1: constant:= -3;
    h1: constant:=  9;
    l2: constant:= 1;
    h2: constant:= 6;
    type T1 is array(l1..h1, l2..h2) of Integer;
    a: T1;
    b: array(l1..h1, l2..h2) of Real;
    type T2 is array(6..9) of Integer;
    type T3 is record x: Integer; y: T2; end record;
    c: array(l1..h1, l2..h2) of T3;
  begin
    for step in 1..6 loop  --  !! HAC: compiles 1..7 OK without "when 7", interpreter crashes
      Put("Multidimensional array: Step");
      Put(step);
      New_Line;
      for i in l1..h1 loop
        for j in l2..h2 loop
          case step is
            when 1 =>  --  fill array a
              a(i,j):= i * j;
            when 2 =>  --  display array a
              Put(a(i,j));
              if j = h2 then
                New_Line;
              end if;
            when 3 =>  --  fill array b
              --  HAC 0.01 accepted "b(i,j):= i * j" without Real(...);
              b(i,j):= Real(i * j);
            when 4 =>  --  display array b
              Put(b(i,j), 5, 1, 0);  --  Fore, Aft, Exp.
              Put(' ');
              if j = h2 then
                New_Line;
              end if;
            when 5 =>  --  fill array c
              c(i,j).y(7):= i * j;
            when 6 =>  --  display array c
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
  hs: String (1 .. 7);

  procedure Show_hs is
  begin
    Put_Line (hs);
  end Show_hs;

begin
  Put('A');
  c:= 'B';
  Put(c);
  c:= ''';
  Put(c);
  Put('"');
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
  New_Line;
  Put(Twenty); Put_Line(" ... should be: 20");
  Put(Ten_point_one); Put_Line(" ... should be: 1.01000E+01");
  x_glob:= 123;
  z1.x:= Add(432, x_glob); -- then: z1.x = 555
  Put(z1.x);
  Put_Line(" ... should be: 555");
  z1.y:= 1.8;
  Put(z1.y); Put_Line(" ... should be: 1.80000E+00");
  Put_Line("Do_0: output 54321");
  Do_0; Put(x_glob); New_Line;
  Put_Line("6-3+2 = 5");
  Put(6-3+2); New_Line;
  --
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
  for gagl in 1 .. 20 loop
    Put(gagl);
    Put(" choice : ");
    case gagl is
      when 4      => Put_Line ("... four");
      when 2      => Put_Line ("... two");
      when 7      => Put_Line ("... seven");
      when others => Put (gagl, 1); Put_Line (" (others)");
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
