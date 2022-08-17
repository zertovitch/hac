--  Output should be empty if the compiler is correct.

with HAT;
with Testing_Utilities;

procedure Floats is
  use HAT, Testing_Utilities;

  procedure Test_Exp_Log is
    scale : constant := 500.0;
    steps : constant := 100;
    x1, x2 : Real;
  begin
    for i in 0 .. steps loop
      x1 := Real (i) * scale * (1.0 / Real (steps));
      x2 := Log (Exp (x1));
      Assert (abs (x2 - x1) <= 1.0e-15, +"Compiler bug [Floats, Exp_Log]");
    end loop;
  end Test_Exp_Log;

  procedure Test_Trigo is
    scale : constant Real := Pi * 0.25;
    steps : constant := 100;
    x, s, c, t : Real;
  begin
    for i in 0 .. steps loop
      x := Real (i) * scale * (1.0 / Real (steps));
      s := Sin (x);
      c := Cos (x);
      if abs c > 0.0 then
        t := s / c;
        Assert (abs (Arctan (t) - x) <= 1.0e-15, +"Compiler bug [Floats, Trigo]");
      end if;
    end loop;
  end Test_Trigo;

  x1 : Real;
  x2 : Real;
  x3 : Real;

  type R is record
    x1 : Real;
    x2 : Real;
    x3 : Real;
  end record;

  ww : array (1 .. 7) of R;

  v : R;

  neg_float_value : constant := -5.07;

  procedure Base_Test is
    --
    function Almost_equal (x, y : Real) return Boolean is
      eps : constant := 1.0e-14;  --  = 9.99999... * 10**(-Real'Digits).
      --  2.22044604925031E-16 = Real'Base'Model_Epsilon
      tol : Real := eps;
      z, ax, ay, ma : Real;
      eq : Boolean;
    begin
      z := abs (x - y);
      ax := abs x;
      ay := abs y;
      ma := ax;
      if ay < ma then ma := ay; end if;
      --  ma is the minimum of absolute values of x and y.
      tol := tol * ma;  --  Relative tolerance
      eq := z <= tol;
      Assert (eq,
               +"Bug [Base_Test " &
               Real'Image (x) & ", " &
               Real'Image (y) & ", " &
               Real'Image (x - y) & ", " &
               Real'Image (tol) & "]  ");
      return eq;
    end Almost_equal;
  begin
    --  Even with the same Real'Image, the difference in parsing (HAC for 10-base numbers,
    --  GNAT otherwise) induces differences which are beyond Real's precision, but large
    --  enough to have a non-zero z in computation.
    if not Almost_equal (10#920.4323#e-20, 9.20432300000000E-18) then Put_Line ("[A]"); end if;
    if not Almost_equal (10#8793.0#e-13, 8.79300000000000E-10)   then Put_Line ("[B]"); end if;
    --
    --  Here we have pasted from the output of floats.adb,
    --  with:   Produce_Base_Test (True);
    --
    if not Almost_equal (+13#62A.A#e+19,  1.53614085896374E+24) then Put_Line ("[1]"); end if;
    if not Almost_equal (+9#4458.4818#e17,  5.49270617585333E+19) then Put_Line ("[2]"); end if;
    if not Almost_equal (-13#19A.C7206#e7, -1.86341399270000E+10) then Put_Line ("[3]"); end if;
    if not Almost_equal (-6#3153.21#e-12, -3.29551144938688E-07) then Put_Line ("[4]"); end if;
    if not Almost_equal (-8#60.65#, -4.88281250000000E+01) then Put_Line ("[5]"); end if;
    if not Almost_equal (-16#62.17#, -9.80898437500000E+01) then Put_Line ("[6]"); end if;
    if not Almost_equal (+6#15.32111#e7,  3.23636400000000E+06) then Put_Line ("[7]"); end if;
    if not Almost_equal (-14#5.B7#e9, -1.20276808064000E+11) then Put_Line ("[8]"); end if;
    if not Almost_equal (-2#11.0#e-16, -4.57763671875000E-05) then Put_Line ("[9]"); end if;
    if not Almost_equal (+2#111.100#e+13,  6.14400000000000E+04) then Put_Line ("[10]"); end if;
    if not Almost_equal (-4#31331.311#e+16, -3.83896256512000E+12) then Put_Line ("[11]"); end if;
    if not Almost_equal (7#6353.1#e3,  7.69398000000000E+05) then Put_Line ("[12]"); end if;
    if not Almost_equal (9#54.74871#,  4.98392182763468E+01) then Put_Line ("[13]"); end if;
    if not Almost_equal (13#22548.A7#,  6.24218106508876E+04) then Put_Line ("[14]"); end if;
    if not Almost_equal (-2#1011.000#e9, -5.63200000000000E+03) then Put_Line ("[15]"); end if;
    if not Almost_equal (+12#48535.8A84#,  9.75297409336420E+04) then Put_Line ("[16]"); end if;
    if not Almost_equal (2#0.01#,  2.50000000000000E-01) then Put_Line ("[17]"); end if;
    if not Almost_equal (10#985.84#e-1,  9.85840000000000E+01) then Put_Line ("[18]"); end if;
    if not Almost_equal (-2#11.101#, -3.62500000000000E+00) then Put_Line ("[19]"); end if;
    if not Almost_equal (-2#10.1001#e19, -1.34348800000000E+06) then Put_Line ("[20]"); end if;
    if not Almost_equal (4#11.32000#,  5.87500000000000E+00) then Put_Line ("[21]"); end if;
    if not Almost_equal (+10#437.94309#,  4.37943090000000E+02) then Put_Line ("[22]"); end if;
    if not Almost_equal (16#70.461D#,  1.12273880004883E+02) then Put_Line ("[23]"); end if;
    if not Almost_equal (+13#A8.26877#,  1.38193254384004E+02) then Put_Line ("[24]"); end if;
    if not Almost_equal (-3#22120.1000#e7, -5.05926000000000E+05) then Put_Line ("[25]"); end if;
    if not Almost_equal (-2#10.10101#e+12, -1.08800000000000E+04) then Put_Line ("[26]"); end if;
    if not Almost_equal (+13#64C4.7869#e8,  1.14353935858760E+13) then Put_Line ("[27]"); end if;
    if not Almost_equal (-7#32465.145#e6, -9.56749794000000E+08) then Put_Line ("[28]"); end if;
    if not Almost_equal (+14#696C6.B05#,  2.56542787536443E+05) then Put_Line ("[29]"); end if;
    if not Almost_equal (-13#454B.90#e-8, -1.18871240938495E-05) then Put_Line ("[30]"); end if;
    if not Almost_equal (-3#0022.2101#, -8.79012345679012E+00) then Put_Line ("[31]"); end if;
    if not Almost_equal (-6#32012.22#e-18, -4.26190501736889E-11) then Put_Line ("[32]"); end if;
    if not Almost_equal (+10#19586.80#,  1.95868000000000E+04) then Put_Line ("[33]"); end if;
    if not Almost_equal (-7#355.052#, -1.87107871720117E+02) then Put_Line ("[34]"); end if;
    if not Almost_equal (-16#79D6.5339E#e+8, -1.33961426264064E+14) then Put_Line ("[35]"); end if;
    if not Almost_equal (+7#1.10#,  1.14285714285714E+00) then Put_Line ("[36]"); end if;
    if not Almost_equal (13#1.011#,  1.00637232589895E+00) then Put_Line ("[37]"); end if;
    if not Almost_equal (-11#50.2938#e6, -9.78946870000000E+07) then Put_Line ("[38]"); end if;
    if not Almost_equal (+5#220.020#,  6.00800000000000E+01) then Put_Line ("[39]"); end if;
    if not Almost_equal (2#0.1#,  5.00000000000000E-01) then Put_Line ("[40]"); end if;
    if not Almost_equal (-9#31351.26#, -2.07012962962963E+04) then Put_Line ("[41]"); end if;
    if not Almost_equal (-10#1.471#e-20, -1.47100000000000E-20) then Put_Line ("[42]"); end if;
    if not Almost_equal (-16#E5744.2#, -9.39844125000000E+05) then Put_Line ("[43]"); end if;
    if not Almost_equal (-12#439.7#e+11, -4.61841619746816E+14) then Put_Line ("[44]"); end if;
    if not Almost_equal (-16#535ED.6ACFC#, -3.41485417232513E+05) then Put_Line ("[45]"); end if;
    if not Almost_equal (+9#3.2347#,  3.26581313824112E+00) then Put_Line ("[46]"); end if;
    if not Almost_equal (13#31A3.189C5#,  6.89312879047006E+03) then Put_Line ("[47]"); end if;
    if not Almost_equal (-14#39C.513B#e18, -3.10069271780268E+23) then Put_Line ("[48]"); end if;
    if not Almost_equal (+3#2.0210#,  2.25925925925926E+00) then Put_Line ("[49]"); end if;
    if not Almost_equal (7#4513.415#e0,  1.62760641399417E+03) then Put_Line ("[50]"); end if;
  end Base_Test;

  procedure Produce_Base_Test (do_it : Boolean) is
    digitz : constant String (1 .. 16) := "0123456789ABCDEF";
    function Rand_Digits (base : Integer) return VString is
      r : VString := +"";
    begin
      for d in 1 .. 1 + Rand (4) loop
        r := r & digitz (1 + Rand (base - 1));
      end loop;
      return r;
    end Rand_Digits;
    b : Integer;
    n, nn : VString;
  begin
    if not do_it then
      return;  --  We stay silent
    end if;
    for it in 51 .. 100 loop
      n := +"";
      case Rand (2) is
        when 0 => n := n & '+';
        when 1 => n := n & '-';
        when others => null;
      end case;
      b := 2 + Rand (14);
      --  TBD: could it be that GNAT is less accurate for non-10 bases ?...
      n := n & b & (+"#") & Rand_Digits (b) & '.' & Rand_Digits (b) & '#';
      if Rnd > 0.5 then
        n := n & 'e';
        case Rand (2) is
          when 0 => n := n & '+';
          when 1 => n := n & '-';
          when others => null;
        end case;
        n := n & Rand (20);
      end if;
      nn := +Real'Image (Float_Value (n));
      Put_Line ("    if not Almost_equal (" & n & (+", ") & nn &
                ") then Put_Line (""[" & it & "]""); end if;");
    end loop;
  end Produce_Base_Test;

  --  This is a copy of an example (see "exm/three_lakes_s.adb") of a
  --  deterministic dynamic system (in this case, a differential equation).
  --  Here, we check the end result of the calculation.
  --
  procedure Three_Lakes_S is
    type Lake is (Morat, Neuchatel, Bienne);
    type Lake_Vector is array (Lake) of Real;

    procedure Times (l : Real; v : Lake_Vector; r : out Lake_Vector) is
    begin
      for i in Lake loop r (i) := v (i) * l; end loop;
    end Times;

    procedure Plus (a, b : Lake_Vector; r : out Lake_Vector) is
    begin
      for i in Lake loop r (i) := a (i) + b (i); end loop;
    end Plus;

    function Sign (i : Real) return Real is
    begin
      if    i < 0.0 then  return -1.0;
      elsif i = 0.0 then  return  0.0;
      else                return  1.0;
      end if;
    end Sign;

    ivs : Lake_Vector;

    procedure Init_Sensitivity is
    begin
      ivs (Morat)     := 1.0 / 2.2820e7;
      ivs (Neuchatel) := 1.0 / 2.1581e8;
      ivs (Bienne)    := 1.0 / 4.0870e7;
    end Init_Sensitivity;

    procedure Evolution (x : in out Lake_Vector; q_e : Lake_Vector; q_sb, h : Real) is

      procedure f (x : Lake_Vector; r : out Lake_Vector) is
        q_tr_mn, q_tr_nb : Real;
        --
        procedure Flux_tansfert is
        begin
          q_tr_mn :=
            --  Canal de la Broye: Morat -> Neuchatel.
            Sign (x (Morat) - x (Neuchatel)) *                       --  sens d'ecoulement
            15.223 *                                                 --  facteur de debit
            (((x (Morat) + x (Neuchatel)) * 0.5 - 426.0)**1.868) *   --  effet du niveau moyen
            ((abs (x (Morat) - x (Neuchatel)))**0.483);      --  effet de la diff. de niveaux
          --
          q_tr_nb :=
            --  Canal de la Thielle: Neuchatel -> Bienne.
            Sign (x (Neuchatel) - x (Bienne)) *                      --  sens d'ecoulement
            18.582 *                                                 --  facteur de debit
            (((x (Neuchatel) + x (Bienne)) * 0.5 - 426.0)**2.511) *  --  effet du niveau moyen
            ((abs (x (Neuchatel) - x (Bienne)))**0.482);     --  effet de la diff. de niveaux
        end Flux_tansfert;
      begin
        Flux_tansfert;
        r (Morat) := (q_e (Morat)     - q_tr_mn) * ivs (Morat);
        r (Neuchatel) := (q_e (Neuchatel) + q_tr_mn - q_tr_nb) * ivs (Neuchatel);
        r (Bienne) := (q_e (Bienne)              + q_tr_nb - q_sb) * ivs (Bienne);
      end f;
      k1, k2, k3, k4, tmp_a, tmp_b, dbk2, dbk3 : Lake_Vector;
    begin
      --  Runge-Kutta, Order 4
      f (x, k1);
      --
      Times (h * 0.5, k1, tmp_a);
      Plus (x, tmp_a, tmp_b);      --  tmp_b = x + h * 0.5 * k1
      f (tmp_b, k2);
      --
      Times (h * 0.5, k2, tmp_a);
      Plus (x, tmp_a, tmp_b);      --  tmp_b = x + h * 0.5 * k2
      f (tmp_b, k3);
      --
      Times (h, k3, tmp_a);
      Plus (x, tmp_a, tmp_b);      --  tmp_b = x + h * k3
      f (tmp_b, k4);
      --
      Times (2.0, k2, dbk2);
      Times (2.0, k3, dbk3);
      Plus (k1, dbk2, tmp_a);
      Plus (tmp_a, dbk3, tmp_b);
      Plus (tmp_b, k4, tmp_a);     --  tmp_a = (k1 + 2.0 * k2 + 2.0 * k3 + k4)
      Times (h * (1.0 / 6.0), tmp_a, tmp_b);
      Plus (x, tmp_b, tmp_a);
      x := tmp_a;
    end Evolution;

    procedure Simulation is
      x, q_e : Lake_Vector;
      q_sb, h : Real;
      n_iter : Integer;

    begin
      h := 3600.0;
      n_iter := 24 * 20;
      x (Morat)     := 428.2;
      x (Neuchatel) := 429.0;
      x (Bienne)    := 429.4;
      q_e (Morat)     := 40.0;
      q_e (Neuchatel) := 70.0;
      q_e (Bienne)    := 100.0;
      q_sb := 200.0;
      for i in 0 .. n_iter loop
        Evolution (x, q_e, q_sb, h);
      end loop;
      Assert (abs (x (Neuchatel) - 429.06377) <= 0.0002, +"Compiler bug [Floats, Three_Lakes_S]");
    end Simulation;

  begin
    Init_Sensitivity;
    Simulation;
  end Three_Lakes_S;

begin
  v.x1 := 1.0;
  v.x2 := 3.0;
  x3 := 5.0;
  v.x3 := 6.0;
  Assert (x3 = 5.0, +"Compiler bug [Floats, A]");
  x1 := v.x1;
  Assert (x1 = 1.0, +"Compiler bug [Floats, B]");
  x3 := v.x2;
  Assert (x3 = 3.0, +"Compiler bug [Floats, C]");
  ww (1).x3 := 3.4_5_6_7_8_9;
  ww (5).x3 := ww (1).x3;
  ww (1).x3 := 7.89;
  v.x3 := 1.0;
  v.x2 := 2.0;
  Assert (abs (ww (5).x3 - (2.345_678 + 1.111111)) <= 0.000_000_1, +"Compiler bug [Floats, D]");
  x2 := neg_float_value;
  Assert (-x2 = 5.07, +"Compiler bug [Floats, E]");  --  Former HAC bug: unary minus was ineffective for floats
  --
  Test_Exp_Log;
  Test_Trigo;
  --
  Produce_Base_Test (False);
  Base_Test;
  Three_Lakes_S;
  --
  Assert (Max (1.0, 2.0) = 2.0, +"HAT.Max");
  Assert (Min (1.0, 2.0) = 1.0, +"HAT.Min");
end Floats;
