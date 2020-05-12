--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Floats is

  procedure Test_Exp_Log is
    scale : constant := 500.0;
    steps : constant := 100;
    x1, x2 : Real;
  begin
    for i in 0 .. steps loop
      x1 := Real (i) * scale * (1.0 / Real (steps));
      x2 := Log (Exp (x1));
      if abs (x2 - x1) > 0.0 then
        Put_Line ("Compiler bug [Exp_Log]");
      end if;
    end loop;
  end Test_Exp_Log;

  procedure Test_Trigo is
    pi : constant := 3.141592653;
    scale : constant Real := pi * 0.25;
    steps : constant := 100;
    x, s, c, t : Real;
  begin
    for i in 0 .. steps loop
      x := Real (i) * scale * (1.0 / Real (steps));
      s := Sin (x);
      c := Cos (x);
      if abs (c) > 0.0 then
        t := s / c;
        if abs (Arctan (t) - x) > 1.0e-15 then
          Put_Line ("Compiler bug [Trigo]");
        end if;
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
      ax := abs (x);
      ay := abs (y);
      ma := ax;
      if ay < ma then ma := ay; end if;
      --  ma is the minimum of absolute values of x and y.
      tol := tol * ma;  --  Relative tolerance
      eq := z <= tol;
      if not eq then
        Put ("Bug [Base_Test " &
               Image_Attribute (x) & ", " &
               Image_Attribute (y) & ", " &
               Image_Attribute (x - y) & ", " &
               Image_Attribute (tol) & "]  ");
      end if;
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
    digitz : constant String (1..16) := "0123456789ABCDEF";
    function Rand_Digits (base : Integer) return VString is
      r : VString := +"";
    begin
      for d in 1 .. 1 + Rand (4) loop
        r := r & digitz (1 + Rand (base - 1));
      end loop;
      return r;
    end;
    b : Integer;
    n, nn : VString;
  begin
    if not do_it then
      return;  --  We stay silent
    end if;
    for it in 1 .. 50 loop
      n := +"";
      case Rand (2) is
        when 0 => n := n & '+';
        when 1 => n := n & '-';
        when others => null;
      end case;
      b := 2 + Rand (14);
      -- TBD: could it be that GNAT is less accurate for non-10 bases ?...
      n := n & b & (+"#") & Rand_Digits (b) & '.' & Rand_Digits (b) & '#';
      if Rnd > 0.5 then
        n := n & 'e';
        case Rand (2) is
          when 0 => n := n & '+';
          when 1 => n := n & '-';
          when others => null;
        end case;
        n := n & Rand(20);
      end if;
      nn := Image_Attribute (Float_Value (n));
      Put_Line ( "    if not Almost_equal (" & n & (+", ") & nn &
                 ") then Put_Line (""[" & it & "]""); end if;");
    end loop;
  end Produce_Base_Test;

begin
  v.x1 := 1.0;
  v.x2 := 3.0;
  x3 := 5.0;
  v.x3 := 6.0;
  if x3 /= 5.0 then
    Put_Line ("Compiler bug [A]");
  end if;
  x1 := v.x1;
  if x1 /= 1.0 then
    Put_Line ("Compiler bug [B]");
  end if;
  x3 := v.x2;
  if x3 /= 3.0 then
    Put_Line ("Compiler bug [C]");
  end if;
  ww (1).x3:= 3.4_5_6_7_8_9;
  ww (5).x3:= ww (1).x3;
  ww (1).x3 := 7.89;
  v.x3 := 1.0;
  v.x2 := 2.0;
  if abs (ww (5).x3 - (2.345_678 + 1.111111)) > 0.000_000_1 then
    Put_Line ("Compiler bug [D]");
  end if;
  x2 := neg_float_value;
  if -x2 /= 5.07 then
    Put_Line ("Compiler bug [E]");  --  Former HAC bug: unary minus was ineffective for floats
  end if;
  --
  Test_Exp_Log;
  Test_Trigo;
  --
  Produce_Base_Test (False);
  Base_Test;
end Floats;
