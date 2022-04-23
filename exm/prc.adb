--  Demo of modularity with packages.

with HAL;
with Pkg_1, Pkg_2;
with Cross_A, Cross_B;

procedure Prc is

  x : Pkg_1.A;

  use Pkg_1;

  y : B;
  z : Pkg_1.E;

  t : constant Pkg_2.G := Pkg_2.G'Last;

  r : HAL.Real;

  use HAL;

begin
  x := 0;
  y := 0;
  z := 2;
  Put (t, 0);
  New_Line;
  Proc_1 (x, r);
  Put (r, 0, 7, 0);
  New_Line;
  Sub_Pkg_2.Proc_2 (+"Hey man");
  Cross_A.A (5);
  Cross_B.B (5);
end Prc;
