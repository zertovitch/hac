--  https://rosettacode.org/wiki/https://rosettacode.org/wiki/Draw_a_sphere

with HAT;

procedure Draw_Sphere is

  use HAT;

  type Point is record
    x, y, z : Real;
  end record;

  shades : constant String (1 .. 10) := "@%#&eo*!:.";

  procedure Normalize (x, y, z : Real; pt : out Point) is
    len : constant Real := Sqrt (x * x + y * y + z * z);
  begin
    pt.x := x / len;
    pt.y := y / len;
    pt.z := z / len;
  end Normalize;

  function dot_product (a, b : Point) return Real is
  begin
    return a.x * b.x + a.y * b.y + a.z * b.z;
  end dot_product;

  light : Point;

  procedure Draw_Sphere (r, k : Integer; ambient : Real) is
    clarity, x, y, z2 : Real;
    surface_point : Point;
    shade_index : Integer;
  begin
    Normalize (30.0, 30.0, -50.0, light);
    for i in -r .. r loop
      x := Real (i) + 0.5;
      for j in (-2) * r .. 2 * r loop
        y := Real (j) / 2.0 + 0.5;
        z2 := Real (r * r) - x * x - y * y;
        if z2 >= 0.0 then
          Normalize (x, y, Sqrt (z2), surface_point);
          clarity := Max (0.0, -dot_product (light, surface_point)) ** k + ambient;
          shade_index := shades'First +  Integer (clarity * Real (shades'Length));
          --  Clamp:
          shade_index := Min (shades'Last, Max (shades'First, shade_index));
          Put (shades (shade_index));
        else
          Put (' ');
        end if;
      end loop;
      New_Line;
    end loop;
  end Draw_Sphere;

begin
  Draw_Sphere (20, 4, 0.1);
  Draw_Sphere (10, 2, 0.4);
  Draw_Sphere  (8, 2, 0.1);
  Draw_Sphere  (8, 1, 0.4);
end Draw_Sphere;
