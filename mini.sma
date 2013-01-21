with SMALL_SP;  use SMALL_SP;

procedure Mini is
  Twenty: constant:= 20;
  x: Integer;
  Ten_point_one: constant := 10.1;
  --
  type Type1 is record
    x: Integer;
    y: Float;
  end record;
  --
  z1: Type1;
begin
  x:= 1234;
  z1.x:= 1;
  z1.y:= 1.8;
end Mini;

