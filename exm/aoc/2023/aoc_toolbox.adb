package body AoC_Toolbox is

  procedure GCD_and_Bezout (a, b : in Integer; s, t, the_gcd : out Integer) is
    --  Finds the GCD and s, t for the
    --  ` GCD (a, b) = a * s + b * t ` factorization (Bezout theorem).
    --  Program 1.8, Introduction to number theory, RBJT Allenby & EJ Redfern
    ta, tb : array (1 .. 3) of Integer;
    q, r : Integer;
  begin
    ta (1) := 1;         tb (1) := 0;
    ta (2) := 0;         tb (2) := 1;
    ta (3) := a;         tb (3) := b;
    while tb (3) /= 0 loop
      q := ta (3) / tb (3);
      for i in 1 .. 3 loop
        r := ta (i) - q * tb (i);
        ta (i) := tb (i);
        tb (i) := r;
      end loop;
    end loop;
    s :=       ta (1);
    t :=       ta (2);
    the_gcd := ta (3);
  end GCD_and_Bezout;

  function GCD (a, b : Integer) return Integer is
    s, t, the_gcd : Integer;
  begin
    GCD_and_Bezout (a, b, s, t, the_gcd);
    return the_gcd;
  end GCD;

  function LCM (a, b : Integer) return Integer is
  begin
    return abs (a * b) / GCD (a, b);
  end LCM;

  function Dist_L1 (a, b : Point) return Natural is
  begin
    return
      abs (a.x - b.x) + abs (a.y - b.y);
  end Dist_L1;

  function Dist_Max (a, b : Point) return Natural is
  begin
    return
      HAT.Max (abs (a.x - b.x), abs (a.y - b.y));
  end Dist_Max;

  procedure Skip_till_Space (fff : in out HAT.File_Type; times : Positive) is
    c : Character;
    use HAT;
  begin
    for repeat in 1 .. times loop
      while not End_Of_File (fff) loop
        Get (fff, c);
        exit when c = ' ';
      end loop;
      exit when End_Of_File (fff);
    end loop;
  end Skip_till_Space;

  function Sgn_64 (iii : Interfaces.Integer_64) return Interfaces.Integer_64 is
    use Interfaces;
  begin
    if iii > 0 then
      return 1;
    elsif iii < 0 then
      return -1;
    else
      return 0;
    end if;
  end Sgn_64;

  procedure Rotate (x, y : in out HAT.Real; a : HAT.Real) is
    use HAT;
    nx : Real;
  begin
    nx := Cos (a) * x - Sin (a) * y;
    y  := Sin (a) * x + Cos (a) * y;
    x  := nx;
  end Rotate;

  function Deg_2_Rad (a : HAT.Real) return HAT.Real is
    use HAT;
  begin
    return (Pi / 180.0) * a;
  end Deg_2_Rad;

end AoC_Toolbox;
