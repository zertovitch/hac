package AoC_Toolbox is

  type Point is record
    x, y : Integer;
  end record;

  function Dist_L1 (a, b : Point) return Natural;

  function Dist_Max (a, b : Point) return Natural;

end AoC_Toolbox;
