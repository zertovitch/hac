--  Output must be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Constants is

  n1, n2 : constant := 9.0;  --  Numeric constant (universal float)
  n3 : constant := 1.0;

  pi : constant := 3.14159265358979323846264338327950288419716939937510;
  minus_pi : constant := -pi;

  r1 : Real;
  r2 : Real := 123.0;
  r3 : constant Real := 456.0;

  i1 : constant Integer := 7;

  type R is record a,b : Integer; end record;

  s1 : R;
  s2 : R := s1;
  s3 : constant R := s2;

  --  wrong1 : ;  --  untyped constant
  --  wrong2 : constant;
  --  wrong3 : constant Integer;

  procedure Test_In (x : in R) is null;
  procedure Test_Out (x : out R) is null;
  procedure Test_In_Out (x : in out R) is null;

  procedure Test_12_34 (x : R) is
    y : constant R := x;
  begin
    if (y.a /= 12) or (y.b /= 34) then
      Put_Line ("Compiler bug [B]");
    end if;
    --  x.a := 666;  --  must be rejected (cannot modify "in" parameter)
    --  y.a := 666;  --  must be rejected (cannot modify constant)
    Test_In (x);
    --  Test_Out (y);     --  must be rejected (cannot modify constant)
    --  Test_In_Out (x);  --  must be rejected (cannot modify constant)
  end;

  type Animal is (ant, bat, cat, dog);

  pet : constant Animal := dog;

begin
  if n1 + n2 - 17.0 /= n3 then
    Put_Line ("Compiler bug [A]");
  end if;
  r2 := 7.0;
  --  s3.a := 4;  --  must be rejected (cannot modify constant)
  --  i1 := 6;    --  must be rejected (cannot modify constant)
  --  r3 := 6.0;  --  must be rejected (cannot modify constant)
  s1.a := 12;
  s1.b := 34;
  Test_12_34 (s1);
  for i in 1 .. 5 loop
    null;
    --  i := 1;  --  must be rejected (cannot modify constant)
  end loop;
end Constants;
