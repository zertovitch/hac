with HAT, Interfaces;

package AoC_Toolbox is

  ----------------
  --  AoC game  --
  ----------------

  type Part_Type is (part_1, part_2);

  type Data_Type is (mini, input);

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

  procedure Rotate (x, y : in out HAT.Real; a : HAT.Real);

  function Deg_2_Rad (a : HAT.Real) return HAT.Real;

end AoC_Toolbox;
