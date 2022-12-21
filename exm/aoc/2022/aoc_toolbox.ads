with HAT, Interfaces;

package AoC_Toolbox is

  ----------------------
  --  Plane Geometry  --
  ----------------------

  type Point is record
    x, y : Integer;
  end record;

  function Dist_L1 (a, b : Point) return Natural;

  function Dist_Max (a, b : Point) return Natural;

  -----------------------
  --  Text processing  --
  -----------------------

  procedure Skip_till_Space (f : in out HAT.File_Type; times : Positive);

  --------------------
  --  Miscellaneous --
  --------------------

  function Sgn_64 (i : Interfaces.Integer_64) return Interfaces.Integer_64;

end AoC_Toolbox;
