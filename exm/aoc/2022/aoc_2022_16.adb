--  Solution to Advent of Code 2022, Day 16
-------------------------------------------
--  Proboscidea Volcanium
--
--  https://adventofcode.com/2022/day/16
--  Copy of questions in: aoc_2022_16_questions.txt
--
--  Note: this solution takes an insane amount of time with HAC.
--  Fortunately, you can compile it with GNAT and
--  the total run-time is there 0.94 seconds on an i7 machine.

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_16 is
  use HAT;

  type Valve is
    (nil,
     AA, AB, AC, AD, AE, AF, AG, AH, AI, AJ, AK, AL, AM, AN, AO, AP, AQ, AR, AS,     AU, AV, AW, AX, AY, AZ,
     BA, BB, BC, BD, BE, BF, BG, BH, BI, BJ, BK, BL, BM, BN, BO, BP, BQ, BR, BS, BT, BU, BV, BW, BX, BY, BZ,
     CA, CB, CC, CD, CE, CF, CG, CH, CI, CJ, CK, CL, CM, CN, CO, CP, CQ, CR, CS, CT, CU, CV, CW, CX, CY, CZ,
     DA, DB, DC, DD, DE, DF, DG, DH, DI, DJ, DK, DL, DM, DN,     DP, DQ, DR, DS, DT, DU, DV, DW, DX, DY, DZ,
     EA, EB, EC, ED, EE, EF, EG, EH, EI, EJ, EK, EL, EM, EN, EO, EP, EQ, ER, ES, ET, EU, EV, EW, EX, EY, EZ,
     FA, FB, FC, FD, FE, FF, FG, FH, FI, FJ, FK, FL, FM, FN, FO, FP, FQ, FR, FS, FT, FU, FV, FW, FX, FY, FZ,
     GA, GB, GC, GD, GE, GF, GG, GH, GI, GJ, GK, GL, GM, GN, GO, GP, GQ, GR, GS, GT, GU, GV, GW, GX, GY, GZ,
     HA, HB, HC, HD, HE, HF, HG, HH, HI, HJ, HK, HL, HM, HN, HO, HP, HQ, HR, HS, HT, HU, HV, HW, HX, HY, HZ,
     IA, IB, IC, ID, IE,     IG, IH, II, IJ, IK, IL, IM,     IO, IP, IQ, IR,     IT, IU, IV, IW, IX, IY, IZ,
     JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ, JK, JL, JM, JN, JO, JP, JQ, JR, JS, JT, JU, JV, JW, JX, JY, JZ,
     KA, KB, KC, KD, KE, KF, KG, KH, KI, KJ, KK, KL, KM, KN, KO, KP, KQ, KR, KS, KT, KU, KV, KW, KX, KY, KZ,
     LA, LB, LC, LD, LE, LF, LG, LH, LI, LJ, LK, LL, LM, LN, LO, LP, LQ, LR, LS, LT, LU, LV, LW, LX, LY, LZ,
     MA, MB, MC, MD, ME, MF, MG, MH, MI, MJ, MK, ML, MM, MN, MO, MP, MQ, MR, MS, MT, MU, MV, MW, MX, MY, MZ,
     NA, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK, NL, NM, NN, NO, NP, NQ, NR, NS, NT, NU, NV, NW, NX, NY, NZ,
     OA, OB, OC, OD, OE,     OG, OH, OI, OJ, OK, OL, OM, ON, OO, OP, OQ,     OS, OT, OU, OV, OW, OX, OY, OZ,
     PA, PB, PC, PD, PE, PF, PG, PH,     PJ, PK, PL, PM, PN, PO, PP, PQ, PR, PS, PT, PU, PV, PW, PX, PY, PZ,
     QA, QB, QC, QD, QE, QF, QG, QH, QI, QJ, QK, QL, QM, QN, QO, QP, QQ, QR, QS, QT, QU, QV, QW, QX, QY, QZ,
     RA, RB, RC, RD, RE, RF, RG, RH, RI, RJ, RK, RL, RM, RN, RO, RP, RQ, RR, RS, RT, RU, RV, RW, RX, RY, RZ,
     SA, SB, SC, SD, SE, SF, SG, SH, SI, SJ, SK, SL, SM, SN, SO, SP, SQ, SR, SS, ST, SU, SV, SW, SX, SY, SZ,
     TA, TB, TC, TD, TE, TF, TG, TH, TI, TJ, TK, TL, TM, TN, TO, TP, TQ, TR, TS, TT, TU, TV, TW, TX, TY, TZ,
     UA, UB, UC, UD, UE, UF, UG, UH, UI, UJ, UK, UL, UM, UN, UO, UP, UQ, UR, US, UT, UU, UV, UW, UX, UY, UZ,
     VA, VB, VC, VD, VE, VF, VG, VH, VI, VJ, VK, VL, VM, VN, VO, VP, VQ, VR, VS, VT, VU, VV, VW, VX, VY, VZ,
     WA, WB, WC, WD, WE, WF, WG, WH, WI, WJ, WK, WL, WM, WN, WO, WP, WQ, WR, WS, WT, WU, WV, WW, WX, WY, WZ,
     XA, XB, XC, XD, XE, XF, XG, XH, XI, XJ, XK, XL, XM, XN, XO, XP, XQ, XR, XS, XT, XU, XV, XW, XX, XY, XZ,
     YA, YB, YC, YD, YE, YF, YG, YH, YI, YJ, YK, YL, YM, YN, YO, YP, YQ, YR, YS, YT, YU, YV, YW, YX, YY, YZ,
     ZA, ZB, ZC, ZD, ZE, ZF, ZG, ZH, ZI, ZJ, ZK, ZL, ZM, ZN, ZO, ZP, ZQ, ZR, ZS, ZT, ZU, ZV, ZW, ZX, ZY, ZZ);

  flow : array (Valve) of Integer;

  next : array (Valve, 1 .. 5) of Valve;

  verbose : constant Natural := 0;

  procedure Data_Acquisition is
    c1, c2, sep : Character;
    valve_nr        : String (1 ..  6) := "Valve ";
    has_flow_rate   : String (1 .. 15) := " has flow rate=";
    tunnels_lead_to : String (1 .. 24) := "; tunnel lead to valves ";  --  "tunnel" can be with or without 's'
    f : File_Type;
    val : Valve;
  begin
    for i in Valve loop
      flow (i) := 0;
      for j in next'Range (2) loop
        next (i, j) := nil;
      end loop;
    end loop;
    Open (f, "aoc_2022_16.txt");
  Read_Data :
    while not End_Of_File (f) loop
      Get (f, valve_nr);
      Get (f, c1);
      Get (f, c2);
      val := Valve'Value (c1 & c2);
      if verbose > 0 then
        Put (val'Image & ": ");
      end if;
      Get (f, has_flow_rate);
      Get (f, flow (val));
      if verbose > 0 then
        Put (flow (val), 3);
        Put ("  -> ");
      end if;
      Get (f, tunnels_lead_to);
      for j in next'Range (2) loop
        Get (f, sep);
        if sep = ' ' then
          Get (f, c1);
        else
          c1 := sep;
        end if;
        Get (f, c2);
        next (val, j) := Valve'Value (c1 & c2);
        if verbose > 0 then
          Put ("  " & next (val, j)'Image);
        end if;
        exit when End_Of_Line (f);
        Get (f, sep);
      end loop;
      if verbose > 0 then
        New_Line;
      end if;
    end loop Read_Data;
    Close (f);
  end Data_Acquisition;

  --  The following "laziness filter" is very efficient at
  --  cutting the recursion for paths that are obviously
  --  unefficient. For solving part 2 (man + elephant), the
  --  run time goes from 12+ hours to 6 seconds on a lame
  --  laptop even if the filter is filled in a very rough way.
  --  The run-time for part 1 & 2 is less than one second
  --  on an i7 machine.
  --
  --  TBD: fill the filter in a non-arbitrary way, using
  --  for instance the ditance of a valve to the AA point
  --  and its potential score.

  laziness_filter : array (0 .. 30) of Integer;

  procedure Fill_Laziness_Filter_Affine_Function
    (floor_final_score         : Natural;
     --  ^ Final score must be at least that value.
     max_time_left_for_0_score : Natural)
     --  ^ Allow score to be still 0 when time left if that amount or more
  is
  begin
    for t in laziness_filter'Range loop
      --  Here we have an unsophisticated affine function.
      --  Score increments are small for small values of time left.
      --  Score increments must be large at the beginning of
      --  the journey in order to maximize the result and compensate
      --  for the first lazy steps where no valve is opened.
      --  So on both ends the affine function is on the safer side.
      --  However, it is empirical and we have no proof that the optimal
      --  is not filtered out by this function.
      laziness_filter (t) :=
        floor_final_score
          - t * (floor_final_score / max_time_left_for_0_score);
    end loop;
  end Fill_Laziness_Filter_Affine_Function;

  function Visit_One_Agent (v, from : Valve; time_left, score : Natural) return Natural is
    mem_flow : Positive;
    result_opening, result_moving : Natural := 0;
    next_valve : Valve;
  begin
    if time_left = 0 then
      if verbose > 1 then
        Put_Line ("Time exhausted!");
        for v in Valve loop
          if flow (v) > 0 then
            Put (v'Image & ": ");
            Put (flow (v), 3);
            New_Line;
          end if;
        end loop;
      end if;
      --  No time left.
      return 0;
    end if;
    if score < laziness_filter (time_left) then
      return 0;
    end if;
    if flow (v) > 0 then
      --  Try a variant where we open the valve v.
      mem_flow := flow (v);
      result_opening := flow (v) * (time_left - 1);
      --  ^ The flow begins only after one minute for
      --    opening the valve.
      flow (v) := 0;
      result_opening :=
        result_opening + Visit_One_Agent (v, nil, time_left - 1, score + result_opening);
      --  ^ The recursive visit includes moving further.
      --    We forget the previous node's location since
      --    it is meaningful to walk back in that case:
      --    purpose was for opening a valve.
      --
      --  Restore previous state:
      flow (v) := mem_flow;
    end if;
    --  Move to adjacent nodes:
    for j in next'Range (2) loop
      next_valve := next (v, j);
      exit when next_valve = nil;
      if next_valve /= from then
        --  ^ Avoid hesitant moves (going back right
        --    after coming and having done nothing).
        result_moving :=
          Max (result_moving, Visit_One_Agent (next_valve, v, time_left - 1, score));
      end if;
    end loop;
    return Max (result_opening, result_moving);
  end Visit_One_Agent;

  function Visit_Two_Agents
    (v_m, from_m, v_e, from_e : Valve;
    time_left, score          : Natural)
    return Natural
  is
    mem_flow_m, mem_flow_e : Positive;
    result_opening_m, result_opening_e, result_opening_2,
    result_moving_m, result_moving_e, result_moving_2 : Natural := 0;
    next_valve_m, next_valve_e : Valve;
  begin
    if time_left = 0 or else score < laziness_filter (time_left) then
      return 0;
    end if;
    if flow (v_m) > 0 and then flow (v_e) > 0 and then v_m /= v_e then
      --  Man and Elephant open valves
      mem_flow_m := flow (v_m);
      mem_flow_e := flow (v_e);
      result_opening_2 := (flow (v_m) + flow (v_e)) * (time_left - 1);
      flow (v_m) := 0;
      flow (v_e) := 0;
      result_opening_2 :=
        result_opening_2 +
          Visit_Two_Agents
            (v_m, nil, v_e, nil, time_left - 1, score + result_opening_2);
      flow (v_m) := mem_flow_m;
      flow (v_e) := mem_flow_e;
    end if;
    if flow (v_m) > 0 then
      --  Man opens valve
      mem_flow_m := flow (v_m);
      result_opening_m := flow (v_m) * (time_left - 1);
      flow (v_m) := 0;
      --  Elephant moves
      for j_e in next'Range (2) loop
        next_valve_e := next (v_e, j_e);
        exit when next_valve_e = nil;
        if next_valve_e /= from_e then
          result_moving_e :=
            Max
              (result_moving_e,
               Visit_Two_Agents
                 (v_m, nil, next_valve_e, v_e, time_left - 1, score + result_opening_m));
        end if;
      end loop;
      flow (v_m) := mem_flow_m;
      result_opening_m := result_opening_m + result_moving_e;
    end if;
    if flow (v_e) > 0 then
      --  Elephant opens valve
      mem_flow_e := flow (v_e);
      result_opening_e := flow (v_e) * (time_left - 1);
      flow (v_e) := 0;
      --  Man moves
      for j_m in next'Range (2) loop
        next_valve_m := next (v_m, j_m);
        exit when next_valve_m = nil;
        if next_valve_m /= from_m then
          result_moving_m :=
            Max
              (result_moving_m,
               Visit_Two_Agents
                 (next_valve_m, v_m, v_e, nil, time_left - 1, score + result_opening_e));
        end if;
      end loop;
      flow (v_e) := mem_flow_e;
      result_opening_e := result_opening_e + result_moving_m;
    end if;
    --  Man and Elephant move
    for j_m in next'Range (2) loop
      next_valve_m := next (v_m, j_m);
      exit when next_valve_m = nil;
      if next_valve_m /= from_m then
        for j_e in next'Range (2) loop
          next_valve_e := next (v_e, j_e);
          exit when next_valve_e = nil;
          if next_valve_e /= from_e
            and then not (v_m = v_e and then next_valve_m >= next_valve_e)
            --  If Man and Elephant are in the same room,
            --  we skip symmetrical movements.
          then
            result_moving_2 :=
              Max
                (result_moving_2,
                 Visit_Two_Agents
                   (next_valve_m, v_m, next_valve_e, v_e, time_left - 1, score));
          end if;
        end loop;
      end if;
    end loop;
    return
      Max
        (Max (result_opening_2, result_opening_m),
         Max (result_opening_e, result_moving_2));
  end Visit_Two_Agents;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

begin
  Data_Acquisition;

  Fill_Laziness_Filter_Affine_Function (1700, 25);
  r (1) := Visit_One_Agent (AA, nil, 30, 0);
  Fill_Laziness_Filter_Affine_Function (2270, 20);
  r (2) := Visit_Two_Agents (AA, nil, AA, nil, 26, 0);

  Put_Line (+"Done in: " & (Clock - T0) & " seconds");
  Put_Line (+"Part 1: maximum pressure release . . . . . . . . : " & r (1));
  Put_Line (+"Part 2: maximum pressure release (2 players) . . : " & r (2));
  --  Part 1: validated by AoC: 1728
  --  Part 2: validated by AoC: 2304
end AoC_2022_16;
