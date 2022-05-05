package body Pkg_1 is

  -------------------------------------------------
  --  Sub-packages defined only in Pkg_1's body  --
  -------------------------------------------------

  package Sub_Pkg_3 is
  end Sub_Pkg_3;

  package Sub_Pkg_4 is
    function F return Integer;
    --
    package Spec_which_doesnt_need_any_body is
      subtype Ha is Integer;
    end Spec_which_doesnt_need_any_body;
    --
    --  package Spec_which_needs_a_body_1 is
    --    procedure Ho_1;
    --  end Spec_which_needs_a_body_1;
    --
    --  !! HAC bug: doesn't check for missing body !!
  end Sub_Pkg_4;

  type Some_Useless_Type is record
    useless_1 : Integer;
    useless_2 : VS;       --  Defined in the spec
    useless_3 : PA;       --  Defined in the spec, private part
  end record;

  package body Sub_Pkg_4 is

    package Spec_which_needs_a_body_2 is
      procedure Ho2;
    end Spec_which_needs_a_body_2;

    package body Spec_which_needs_a_body_2 is
      procedure Ho2 is begin HAL.Put ("[Ho2] "); end Ho2;
    end Spec_which_needs_a_body_2;

    --  !! HAC bug: doesn't check for missing body !!

    function F return Integer is
      variable_local_to_F : Some_Useless_Type;
    begin
      Spec_which_needs_a_body_2.Ho2;
      variable_local_to_F.useless_3 := PA'Last;
      return 656 + variable_local_to_F.useless_3;  --  Should be 666.
    end F;

  end Sub_Pkg_4;

  -------------------
  --  Other stuff  --
  -------------------

  procedure Proc_in_body;

  procedure Proc_in_body is
  begin
    null;
  end Proc_in_body;

  procedure Proc_1 (par_1 : Integer; par_2 : out HAL.Real) is
    x, y : PB;

    package Inner_pkg is
      use HAL;
      procedure Ho_ho_ho;
      public_message : constant VString := +"This is public";
    private
      private_message : constant VString := +"Ho ho ";
    end Inner_pkg;

    some_garbage_1 : Integer;

    package body Inner_pkg is
      body_message : constant VString := +"ho!";
      procedure Ho_ho_ho is
      begin
        x.field_2 := private_message & body_message;
      end Ho_ho_ho;
    end Inner_pkg;

    some_garbage_2 : Integer;

    use Inner_pkg;
    use HAL;

  begin
    Inner_pkg.Ho_ho_ho;
    Ho_ho_ho;
    y := x;
    Put_Line (y.field_2);
    --  Put_Line (Inner_pkg.body_message);        --  Compilation should fail if uncommented.
    --  Put_Line (Inner_pkg.private_message);     --  Compilation should fail if uncommented.
    Put_Line (public_message);
    par_2 := Real (Sub_Pkg_4.F) + 0.123456;
  end Proc_1;

  -------------------------------------------------
  --  Sub-packages defined only in Pkg_1's spec  --
  -------------------------------------------------

  package body Sub_Pkg_2 is

    procedure Proc_2 (message : VString) is
    begin
      HAL.Put_Line ("Hello from Sub_Pkg_2.Proc_2: " & message);
    end Proc_2;

  end Sub_Pkg_2;

end Pkg_1;
