--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Constants is

  n1, n2 : constant := 9.0;  --  Numeric constant (universal float)
  n3 : constant := 1.0;

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

  procedure Test_12_34 (x : R) is
    y : constant R := x;
  begin
    if (y.a /= 12) or (y.b /= 34) then
      Put_Line ("Compiler bug [B]");
    end if;
  end;

begin
  if n1 + n2 - 17.0 /= n3 then
    Put_Line ("Compiler bug [A]");
  end if;
  r2 := 7.0;
  s3.a := 4;  --  should be rejected
  i1 := 6;    --  should be rejected
  r3 := 6.0;  --  should be rejected
  s1.a := 12;
  s1.b := 34;
  Test_12_34 (s1);
end Constants;
