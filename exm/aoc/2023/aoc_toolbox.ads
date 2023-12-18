with HAT, Interfaces;

package AoC_Toolbox is

  ----------------
  --  AoC game  --
  ----------------

  type Part_Type is (part_1, part_2);

  type Data_Type is (mini, input);

  subtype Digit_Type is Natural range 0 .. 9;

  ---------------
  --  Algebra  --
  ---------------

  --  Greatest Common Denominator
  --
  procedure GCD_and_Bezout (a, b : in Integer; s, t, the_gcd : out Integer);

  function GCD (a, b : Integer) return Integer;

  --  Least Common Multiple
  --
  function LCM (a, b : Integer) return Integer;

  ----------------------
  --  Plane Geometry  --
  ----------------------

  type Point is record
    x, y : Integer;
  end record;

  function Dist_L1 (a, b : Point) return Natural;

  function Dist_Max (a, b : Point) return Natural;

  procedure Rotate (x, y : in out HAT.Real; a : HAT.Real);

  type Direction_or_Nil is (nil, north, east, south, west);

  subtype Direction is Direction_or_Nil range north .. west;

  function Opposite (d : Direction) return Direction;

  -------------------
  --  3D Geometry  --
  -------------------

  type Point_3D is record
    x, y, z : Integer;
  end record;

  -----------------------
  --  Text processing  --
  -----------------------

  procedure Skip_till_Space (fff : in out HAT.File_Type; times : Positive);

  --  !! HAC bug: fff is visible through USE !!

  --------------------
  --  Miscellaneous --
  --------------------

  function Sgn_64 (iii : Interfaces.Integer_64) return Interfaces.Integer_64;

  --  !! HAC bug: iii is visible through USE !!

  function Deg_2_Rad (a : HAT.Real) return HAT.Real;

end AoC_Toolbox;
