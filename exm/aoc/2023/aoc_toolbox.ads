with HAT, Interfaces;

package AoC_Toolbox is

  use Interfaces;

  ----------------
  --  AoC game  --
  ----------------

  type Part_Type is (part_1, part_2);

  type Data_Type is (mini, input);

  subtype Digit_Type is Natural range 0 .. 9;

  subtype Alpha is Character range 'a' .. 'z';
  subtype Upcase_Alpha is Character range 'A' .. 'Z';

  subtype Binary is Natural range 0 .. 1;

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

  procedure GCD_and_Bezout_64 (a, b : in Integer_64; s, t, the_gcd : out Integer_64);
  function GCD_64 (a, b : Integer_64) return Integer_64;
  function LCM_64 (a, b : Integer_64) return Integer_64;

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

  type Point_3D_R is record
    x, y, z : HAT.Real;
  end record;

  function Dist_L1_3D (a, b : Point_3D) return Natural;
  
  function Dist_L1_3D_R (a, b : Point_3D_R) return HAT.Real;

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

  -----------------
  --  Hash maps  --
  -----------------

  package Hash_Maps is

    type Hash_Slot_Type is record
      key   : HAT.VString;
      value : Integer;
    end record;

    type Hash_Slot_Array_Type is array (1 .. 5) of Hash_Slot_Type;

    type Hash_Box_Type is record
      slot  : Hash_Slot_Array_Type;
      slots : Natural;  --  HAC wish: := 0;
    end record;

    type Hash_Map_Type is array (0 .. 255) of Hash_Box_Type;  --  HAC wish: ideally the only public type...

    procedure Clear (hm : out Hash_Map_Type);

    procedure Insert (hm : in out Hash_Map_Type; key : HAT.VString; value : Integer);

    procedure Find
      (hm              : in out Hash_Map_Type;
       key             :        HAT.VString;
       value           :    out Integer;
       not_found_value :        Integer);

  end Hash_Maps;

end AoC_Toolbox;
