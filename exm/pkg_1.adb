package body Pkg_1 is

  --  Sub-packages defined only in the Pkg_1's body:

  package Sub_Pkg_3 is
  end Sub_Pkg_3;

  package Sub_Pkg_4 is
    function F return Integer;
  end Sub_Pkg_4;

  package body Sub_Pkg_4 is
    function F return Integer is
    begin
      return 666;
    end F;
  end Sub_Pkg_4;

  --

  procedure Proc_in_body;

  procedure Proc_in_body is
  begin
    null;
  end Proc_in_body;

  procedure Proc_1 (par_1 : Integer; par_2 : out HAL.Real) is
    x, y : PB;
    use HAL;
  begin
    x.field_2 := +"Ho ho ho!";
    y := x;
    Put_Line (y.field_2);
    par_2 := Real (Sub_Pkg_4.F) + 0.123456;
  end Proc_1;

  package body Sub_Pkg_2 is

    procedure Proc_2 (message : VString) is
    begin
      HAL.Put_Line ("Hello from Sub_Pkg_2.Proc_2: " & message);
    end Proc_2;

  end Sub_Pkg_2;

end Pkg_1;
