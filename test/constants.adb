--  Output must be empty if the compiler is correct.
--  The order of the declarations is a bit random, it is on purpose.
--  Especially we want to detect eventual memory corruption (buggy compilers).

with HAT;
with Testing_Utilities;

procedure Constants is
  use HAT, Testing_Utilities;

  n1, n2 : constant := 9.0;  --  Numeric constant (universal float)
  n3 : constant := 1.0;

  r1 : Real;
  r2 : Real := 123.0;
  r3 : constant Real := 456.0;

  minus_pi : constant := -Pi;

  i1 : constant Integer := 7;

  type R is record a, b : Integer; end record;

  s1 : R;

  procedure Test_In (x : in R) is null;
  procedure Test_Out (x : out R) is null;
  procedure Test_In_Out (x : in out R) is null;

  procedure Test_12_34 (x : R) is
    y : constant R := x;
  begin
    Assert (y.a = 12 and y.b = 34, +"Compiler bug [Constants, 12_34]");
    --  x.a := 666;  --  must be rejected (cannot modify "in" parameter)
    --  y.a := 666;  --  must be rejected (cannot modify constant)
    Test_In (x);
    --  Test_Out (y);     --  must be rejected (cannot modify constant)
    --  Test_In_Out (x);  --  must be rejected (cannot modify constant)
  end Test_12_34;

  type Animal is (ant, bat, cat, dog);

  pet : constant Animal := dog;

begin
  r1 := 123000.0;
  Assert (Pi + minus_pi = 0.0,                  +"Compiler bug [Constants, Pi - (Minus_Pi)]");
  Assert (r2 * 1000.0 = r1 and r3 - 333.0 = r2, +"Compiler bug [Constants, 123]");
  Assert (n1 + n2 - 17.0 = n3,                  +"Compiler bug [Constants, num const]");
  r2 := 7.0;
  Assert (Integer (r2) = i1, +"Compiler bug [Constants, i1 = 7]");
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
