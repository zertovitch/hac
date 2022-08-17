--  Demo of modularity with packages.

with HAT;
with Pkg_1, Pkg_2;
with Cross_A, Cross_B;

procedure Prc is

  x : Pkg_1.A;

  use Pkg_1;

  y : B;
  z : Pkg_1.E;

  t : constant Pkg_2.G := Pkg_2.G'Last;

  r : HAT.Real;

  use HAT;

  package Local_Namespace is
    procedure Hi;
    --
    package Loc_SubPkg_1 is
      abc : Integer;
    end Loc_SubPkg_1;
    --
    package Loc_SubPkg_1b is
      abc : Integer;
    end Loc_SubPkg_1b;
    --
    package Loc_SubPkg_2 is
      procedure Jedi_2;
    end Loc_SubPkg_2;
    --
  end Local_Namespace;

  package body Local_Namespace is
    --
    procedure Hi is
    begin
      Put_Line ("Hi!");
    end Hi;
    --
    package body Loc_SubPkg_1 is
      --  Superfluous, but admitted as local package.
      --  For library level it would be rejected: Ada RM 7.2 (4))
    end Loc_SubPkg_1;
    --
    --  Loc_SubPkg_3 (spec & body) is fully
    --  contained in Loc_SubPkg_1's body!
    --
    package Loc_SubPkg_3 is
      procedure Jedi_3;
    end Loc_SubPkg_3;
    --
    package body Loc_SubPkg_3 is
      --  !! HAC bug: doesn't check for missing body !!
      procedure Jedi_3 is
      begin
        Put ("[Jedi 3] ");
      end Jedi_3;
    end Loc_SubPkg_3;
    --
    package body Loc_SubPkg_2 is
      procedure Jedi_2 is
      begin
        Loc_SubPkg_3.Jedi_3;
        Put ("[Jedi 2] ");
      end Jedi_2;
    end Loc_SubPkg_2;
    --
  end Local_Namespace;

  procedure Hi_Hi is
    use Local_Namespace;
  begin
    Loc_SubPkg_2.Jedi_2;
    Hi;
  end Hi_Hi;

  procedure Hi_Hi_2 is
    use Local_Namespace;
    use Loc_SubPkg_2;
  begin
    Jedi_2;
    Hi;
  end Hi_Hi_2;

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
  --
  Local_Namespace.Loc_SubPkg_2.Jedi_2;
  Local_Namespace.Hi;
  Hi_Hi;
  Hi_Hi_2;
end Prc;
