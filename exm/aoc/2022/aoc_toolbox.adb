package body AoC_Toolbox is

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

  procedure Skip_till_Space (f : in out HAT.File_Type; times : Positive) is
    c : Character;
    use HAT;
  begin
    for repeat in 1 .. times loop
      while not End_Of_File (f) loop
        Get (f, c);
        exit when c = ' ';
      end loop;
      exit when End_Of_File (f);
    end loop;
  end Skip_till_Space;

  function Sgn_64 (i : Interfaces.Integer_64) return Interfaces.Integer_64 is
    use Interfaces;
  begin
    if i > 0 then
      return 1;
    elsif i < 0 then
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
  begin
    return (HAT.Pi / 180.0) * a;
  end Deg_2_Rad;

end AoC_Toolbox;
