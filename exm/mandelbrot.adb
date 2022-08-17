with HAT; use HAT;

procedure Mandelbrot is

  --  NB: there is a complete Complex package in the Ada library:
  --  Ada.Numerics.Generic_Complex_Types.

  type Complex is record
    Re, Im : Real;
  end record;

  --  function Sqr (c: Complex) return Complex is
  --  !! functions with non-standard return types not yet available for HAC.

  procedure Sqr (c : in out Complex) is
    res : Complex;
  begin
    res.Re := c.Re ** 2 - c.Im ** 2;
    res.Im := 2.0 * c.Re * c.Im;
    c := res;
  end Sqr;

  --  !!  Programmable operators (like "+") are not yet available for HAC.
  procedure Add (c : in out Complex; added : Complex) is
  begin
    c.Re := c.Re + added.Re;
    c.Im := c.Im + added.Im;
  end Add;

  function Square_Modulus (c : Complex) return Real is
  begin
    return c.Re ** 2 + c.Im ** 2;
  end Square_Modulus;

  --

  function Mandelbrot_Iterate (z0 : Complex; max_iter : Integer) return Integer is
    z : Complex;
  begin
    z.Re := 0.0;
    z.Im := 0.0;
    for i in 1 .. max_iter loop
      Sqr (z);
      Add (z, z0);
      if Square_Modulus (z) > 4.0 then
        return i;
      end if;
    end loop;
    return max_iter;
  end Mandelbrot_Iterate;

  --  Display the Mandelbrot set for a given area, in "ASCII art".
  --
  procedure Mandelzoom (x_min, y_min, x_max, y_max : Real) is
    c_max : constant := 98;
    r_max : constant := 20;
    i_max : constant := 24;
    aart : constant String (1 .. i_max) := "0123456789abcdefghijklm ";
    --                                         here we give up ----^
    z0 : Complex;
    width  : constant Real := x_max - x_min;
    height : constant Real := y_max - y_min;
  begin
    for r in 0 .. r_max loop
      for c in 0 .. c_max loop
        z0.Re := x_min + (width  / Real (c_max) * Real (c));
        z0.Im := y_min + (height / Real (r_max) * Real (r));
        Put (aart (Mandelbrot_Iterate (z0, i_max)));
      end loop;
      New_Line;
    end loop;
  end Mandelzoom;

begin
  Mandelzoom (-2.2, -1.0, 0.6, 1.0);
end Mandelbrot;
