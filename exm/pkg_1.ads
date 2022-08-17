with HAT;

package Pkg_1 is

  subtype A is Integer;

  subtype B0 is A;
  subtype B is Pkg_1.A;

  subtype C is B range 0 .. 1000;

  package Sub_Pkg_1 is

    subtype D is C range 1 .. 100;

  end Sub_Pkg_1;

  subtype E is Sub_Pkg_1.D range 2 .. 10;

  procedure Proc_1 (par_1 : Integer; par_2 : out HAT.Real);

  package Sub_Pkg_2 is

    use HAT;

    procedure Proc_2 (message : VString);

  end Sub_Pkg_2;

  subtype VS is HAT.VString;

private

  subtype PA is E;

  type PB is record
    field_1 : PA;
    field_2 : HAT.VString;
  end record;

end Pkg_1;
