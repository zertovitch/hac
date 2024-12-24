--  Solution to Advent of Code 2024, Day 24
-------------------------------------------
--  Crossed Wires
--
--  https://adventofcode.com/2024/day/24
--  Copy of questions in: aoc_2024_24_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

with Interfaces;

procedure AoC_2024_24 is

  use AoC_Toolbox, HAT, Interfaces;

  r : array (Part_Type) of VString;

  procedure Do_Part_1 is
    --  Example:
    --
    --  x00: constant Boolean := true;
    --  x01: constant Boolean := false;
    --  x02: constant Boolean := true;
    --  x03: constant Boolean := true;
    --  x04: constant Boolean := false;
    --  y00: constant Boolean := true;
    --  y01: constant Boolean := true;
    --  y02: constant Boolean := true;
    --  y03: constant Boolean := true;
    --  y04: constant Boolean := true;
    --
    --  djm : constant Boolean := y00 and y03;
    --  ffh : constant Boolean := x03 xor y03;
    --  fgs : constant Boolean := y04 or  y02;
    --  kjc : constant Boolean := x04 and y00;
    --  nrd : constant Boolean := y03 or  x01;
    --  ntg : constant Boolean := x00 xor y04;
    --  pbm : constant Boolean := y01 and x02;
    --  psh : constant Boolean := y03 or  y00;
    --  tnw : constant Boolean := y02 or  x01;
    --  vdt : constant Boolean := x03 or  x00;
    --  fst : constant Boolean := x00 or  x03;
    --
    --  bfw : constant Boolean := vdt or  tnw;
    --  bqk : constant Boolean := ffh or  nrd;
    --  frj : constant Boolean := tnw or  fst;
    --  gnj : constant Boolean := tnw or  pbm;
    --  hwm : constant Boolean := nrd and vdt;
    --  kpj : constant Boolean := pbm or  djm;
    --  kwq : constant Boolean := ntg or  kjc;
    --  mjb : constant Boolean := ntg xor fgs;
    --  qhw : constant Boolean := djm or  pbm;
    --  rvg : constant Boolean := kjc and fst;
    --  tgd : constant Boolean := psh xor fgs;
    --  wpb : constant Boolean := nrd xor fgs;
    --
    --  z00 : constant Boolean := bfw xor mjb;
    --  z01 : constant Boolean := tgd xor rvg;
    --  z02 : constant Boolean := gnj and wpb;
    --  z03 : constant Boolean := hwm and bqk;
    --  z04 : constant Boolean := frj xor qhw;
    --  z05 : constant Boolean := kwq or  kpj;
    --  z06 : constant Boolean := bfw or  bqk;
    --  z07 : constant Boolean := bqk or  frj;
    --  z08 : constant Boolean := bqk or  frj;
    --  z09 : constant Boolean := qhw xor tgd;
    --  z10 : constant Boolean := bfw and frj;
    --  z11 : constant Boolean := gnj and tgd;
    --  z12 : constant Boolean := tgd xor rvg;

    ------

    x00 : constant Boolean := True;
    x01 : constant Boolean := True;
    x02 : constant Boolean := False;
    x03 : constant Boolean := False;
    x04 : constant Boolean := False;
    x05 : constant Boolean := True;
    x06 : constant Boolean := False;
    x07 : constant Boolean := True;
    x08 : constant Boolean := True;
    x09 : constant Boolean := False;
    x10 : constant Boolean := True;
    x11 : constant Boolean := False;
    x12 : constant Boolean := False;
    x13 : constant Boolean := True;
    x14 : constant Boolean := False;
    x15 : constant Boolean := True;
    x16 : constant Boolean := False;
    x17 : constant Boolean := True;
    x18 : constant Boolean := True;
    x19 : constant Boolean := True;
    x20 : constant Boolean := True;
    x21 : constant Boolean := False;
    x22 : constant Boolean := False;
    x23 : constant Boolean := True;
    x24 : constant Boolean := True;
    x25 : constant Boolean := False;
    x26 : constant Boolean := False;
    x27 : constant Boolean := True;
    x28 : constant Boolean := True;
    x29 : constant Boolean := True;
    x30 : constant Boolean := True;
    x31 : constant Boolean := False;
    x32 : constant Boolean := False;
    x33 : constant Boolean := True;
    x34 : constant Boolean := False;
    x35 : constant Boolean := True;
    x36 : constant Boolean := False;
    x37 : constant Boolean := True;
    x38 : constant Boolean := False;
    x39 : constant Boolean := False;
    x40 : constant Boolean := True;
    x41 : constant Boolean := True;
    x42 : constant Boolean := False;
    x43 : constant Boolean := True;
    x44 : constant Boolean := True;
    y00 : constant Boolean := True;
    y01 : constant Boolean := False;
    y02 : constant Boolean := True;
    y03 : constant Boolean := True;
    y04 : constant Boolean := False;
    y05 : constant Boolean := False;
    y06 : constant Boolean := True;
    y07 : constant Boolean := True;
    y08 : constant Boolean := False;
    y09 : constant Boolean := True;
    y10 : constant Boolean := True;
    y11 : constant Boolean := True;
    y12 : constant Boolean := True;
    y13 : constant Boolean := True;
    y14 : constant Boolean := False;
    y15 : constant Boolean := True;
    y16 : constant Boolean := True;
    y17 : constant Boolean := False;
    y18 : constant Boolean := True;
    y19 : constant Boolean := False;
    y20 : constant Boolean := False;
    y21 : constant Boolean := True;
    y22 : constant Boolean := True;
    y23 : constant Boolean := True;
    y24 : constant Boolean := True;
    y25 : constant Boolean := False;
    y26 : constant Boolean := True;
    y27 : constant Boolean := False;
    y28 : constant Boolean := False;
    y29 : constant Boolean := True;
    y30 : constant Boolean := True;
    y31 : constant Boolean := True;
    y32 : constant Boolean := True;
    y33 : constant Boolean := True;
    y34 : constant Boolean := False;
    y35 : constant Boolean := True;
    y36 : constant Boolean := False;
    y37 : constant Boolean := True;
    y38 : constant Boolean := True;
    y39 : constant Boolean := False;
    y40 : constant Boolean := False;
    y41 : constant Boolean := False;
    y42 : constant Boolean := False;
    y43 : constant Boolean := True;
    y44 : constant Boolean := True;

    cvk : constant Boolean := y44 and x44;
    qjp : constant Boolean := y42 and x42;
    kfp : constant Boolean := y41 and x41;
    qhf : constant Boolean := y40 and x40;
    nbc : constant Boolean := y37 and x37;
    prn : constant Boolean := y35 xor x35;
    qph : constant Boolean := y35 and x35;
    rkq : constant Boolean := y34 and x34;
    pmg : constant Boolean := y33 xor x33;
    wrd : constant Boolean := y32 xor x32;
    tkw : constant Boolean := y31 xor x31;
    gpp : constant Boolean := y31 and x31;
    rvc : constant Boolean := y30 and x30;
    qgc : constant Boolean := y30 xor x30;
    qrw : constant Boolean := y27 xor x27;
    fjr : constant Boolean := y27 and x27;
    jkt : constant Boolean := y26 xor x26;
    dbt : constant Boolean := y25 and x25;
    kdn : constant Boolean := y24 xor x24;
    nrw : constant Boolean := y23 and x23;
    sbs : constant Boolean := y21 xor x21;
    vwh : constant Boolean := y20 and x20;
    gpq : constant Boolean := y18 and x18;
    jbp : constant Boolean := y18 xor x18;
    fhn : constant Boolean := y16 and x16;
    vch : constant Boolean := y15 and x15;
    pvj : constant Boolean := y15 xor x15;
    tfh : constant Boolean := y14 xor x14;
    wbw : constant Boolean := y13 and x13;
    wgb : constant Boolean := y13 xor x13;
    bcw : constant Boolean := y10 and x10;
    qpg : constant Boolean := y08 and x08;
    vjc : constant Boolean := y07 and x07;
    gjv : constant Boolean := y07 xor x07;
    knd : constant Boolean := y06 and x06;
    sfn : constant Boolean := y05 xor x05;
    dtn : constant Boolean := y04 and x04;
    ptv : constant Boolean := y04 xor x04;
    rtt : constant Boolean := y03 and x03;
    qpd : constant Boolean := y02 xor x02;
    tng : constant Boolean := y02 and x02;
    jrb : constant Boolean := x44 xor y44;
    bmf : constant Boolean := x43 and y43;
    nsb : constant Boolean := x43 xor y43;
    sbn : constant Boolean := x42 xor y42;
    krk : constant Boolean := x41 xor y41;
    qwt : constant Boolean := x40 xor y40;
    ksf : constant Boolean := x39 xor y39;
    z39 : constant Boolean := x39 and y39;
    spg : constant Boolean := x38 xor y38;
    crp : constant Boolean := x38 and y38;
    vjg : constant Boolean := x37 xor y37;
    pfc : constant Boolean := x36 and y36;
    dkj : constant Boolean := x36 xor y36;
    bgg : constant Boolean := x34 xor y34;
    hbn : constant Boolean := x33 and y33;
    ttt : constant Boolean := x32 and y32;
    bqn : constant Boolean := x29 and y29;
    wtw : constant Boolean := x29 xor y29;
    shp : constant Boolean := x28 and y28;
    rtb : constant Boolean := x28 xor y28;
    jck : constant Boolean := x26 and y26;
    nfm : constant Boolean := x25 xor y25;
    frv : constant Boolean := x24 and y24;
    gcj : constant Boolean := x23 xor y23;
    qgd : constant Boolean := x22 xor y22;
    dpg : constant Boolean := x22 and y22;
    knb : constant Boolean := x21 and y21;
    vdc : constant Boolean := x20 xor y20;
    mcv : constant Boolean := x19 xor y19;
    hhb : constant Boolean := x19 and y19;
    tdb : constant Boolean := x17 xor y17;
    fpk : constant Boolean := x17 and y17;
    vgb : constant Boolean := x16 xor y16;
    ktg : constant Boolean := x14 and y14;
    htr : constant Boolean := x12 xor y12;
    njd : constant Boolean := x12 and y12;
    cbm : constant Boolean := x11 and y11;
    ksb : constant Boolean := x11 xor y11;
    cvv : constant Boolean := x10 xor y10;
    mnm : constant Boolean := x09 xor y09;
    ptc : constant Boolean := x09 and y09;
    jhr : constant Boolean := x08 xor y08;
    hsn : constant Boolean := x06 xor y06;
    fkt : constant Boolean := x05 and y05;
    npk : constant Boolean := x03 xor y03;
    sqk : constant Boolean := x01 xor y01;
    hnj : constant Boolean := x01 and y01;
    z00 : constant Boolean := x00 xor y00;
    jcq : constant Boolean := x00 and y00;

    bmc : constant Boolean := sqk and jcq;
    kpf : constant Boolean := hnj or bmc;
    pwk : constant Boolean := kpf and qpd;
    tpv : constant Boolean := tng or pwk;
    hrr : constant Boolean := tpv and npk;

    bdk : constant Boolean := hrr or rtt;

    nnq : constant Boolean := ptv and bdk;
    svk : constant Boolean := dtn or nnq;
    wnq : constant Boolean := sfn and svk;
    cjh : constant Boolean := fkt or wnq;
    hds : constant Boolean := cjh and hsn;
    wcs : constant Boolean := knd or hds;
    nbb : constant Boolean := gjv and wcs;
    bwm : constant Boolean := nbb or vjc;
    tvw : constant Boolean := bwm and jhr;
    gqb : constant Boolean := tvw or qpg;
    gwh : constant Boolean := mnm xor gqb;
    pgn : constant Boolean := gwh and cvv;
    jbk : constant Boolean := bcw or pgn;
    hgb : constant Boolean := ksb and jbk;
    jgf : constant Boolean := cbm or hgb;
    dmc : constant Boolean := jgf and htr;
    qwr : constant Boolean := dmc or njd;
    dqm : constant Boolean := wbw and qwr;
    nkd : constant Boolean := dqm or wgb;
    jrd : constant Boolean := tfh and nkd;
    bgh : constant Boolean := jrd or ktg;

    vcd : constant Boolean := bgh and pvj;
    nhh : constant Boolean := vcd or vch;
    btr : constant Boolean := vgb and nhh;
    ctv : constant Boolean := btr or fhn;
    ffh : constant Boolean := tdb and ctv;
    wtc : constant Boolean := fpk or ffh;
    pfh : constant Boolean := jbp and wtc;
    ppf : constant Boolean := pfh or gpq;
    gvt : constant Boolean := ppf and mcv;
    tjc : constant Boolean := gvt or hhb;
    vvc : constant Boolean := vdc and tjc;
    kgk : constant Boolean := vwh or vvc;
    rcb : constant Boolean := kgk xor sbs;
    tvj : constant Boolean := rcb or knb;
    vsb : constant Boolean := qgd and tvj;
    grk : constant Boolean := dpg or vsb;
    cdw : constant Boolean := gcj and grk;
    pbv : constant Boolean := nrw or cdw;
    mhd : constant Boolean := kdn and pbv;
    vsw : constant Boolean := frv or mhd;
    dfq : constant Boolean := nfm and vsw;
    kgs : constant Boolean := dfq or dbt;
    wtd : constant Boolean := kgs and jkt;
    mjf : constant Boolean := jck or wtd;
    jgh : constant Boolean := mjf and qrw;
    whc : constant Boolean := fjr or jgh;
    jsf : constant Boolean := rtb and whc;
    vmj : constant Boolean := jsf or shp;
    nbw : constant Boolean := vmj and wtw;
    bqb : constant Boolean := nbw or bqn;
    nth : constant Boolean := qgc and bqb;
    mwg : constant Boolean := nth or rvc;
    vmq : constant Boolean := tkw and mwg;
    mrg : constant Boolean := gpp or vmq;
    vcm : constant Boolean := mrg and wrd;
    rnc : constant Boolean := vcm or ttt;
    dmp : constant Boolean := pmg and rnc;
    kvw : constant Boolean := hbn or dmp;
    gwm : constant Boolean := kvw and bgg;
    bhd : constant Boolean := rkq or gwm;

    drv : constant Boolean := prn and bhd;
    dmj : constant Boolean := drv or qph;
    vnw : constant Boolean := dmj and dkj;
    jgc : constant Boolean := pfc or vnw;
    mqh : constant Boolean := vjg and jgc;
    bwb : constant Boolean := nbc or mqh;

    wjg : constant Boolean := spg and bwb;
    wjf : constant Boolean := crp or wjg;
    hvf : constant Boolean := ksf and wjf;
    jct : constant Boolean := wjf xor ksf;
    nhk : constant Boolean := jct or hvf;
    qwh : constant Boolean := nhk and qwt;
    sgk : constant Boolean := qhf or qwh;
    qvv : constant Boolean := sgk and krk;
    pwt : constant Boolean := kfp or qvv;
    qwg : constant Boolean := sbn and pwt;
    gdt : constant Boolean := qwg or qjp;
    dnc : constant Boolean := nsb and gdt;

    dsk : constant Boolean := gqb and mnm;
    tdh : constant Boolean := dnc or bmf;
    pjg : constant Boolean := jrb and tdh;

    z01 : constant Boolean := jcq xor sqk;
    z02 : constant Boolean := kpf xor qpd;
    z03 : constant Boolean := tpv xor npk;
    z04 : constant Boolean := ptv xor bdk;
    z05 : constant Boolean := svk xor sfn;
    z06 : constant Boolean := cjh xor hsn;
    z07 : constant Boolean := wcs xor gjv;
    z08 : constant Boolean := bwm xor jhr;
    z09 : constant Boolean := dsk or ptc;
    z10 : constant Boolean := cvv xor gwh;
    z11 : constant Boolean := ksb xor jbk;
    z12 : constant Boolean := htr xor jgf;
    z13 : constant Boolean := qwr xor wbw;
    z14 : constant Boolean := nkd xor tfh;
    z15 : constant Boolean := bgh xor pvj;
    z16 : constant Boolean := nhh xor vgb;
    z17 : constant Boolean := ctv xor tdb;
    z18 : constant Boolean := wtc xor jbp;
    z19 : constant Boolean := mcv xor ppf;
    z20 : constant Boolean := vdc xor tjc;
    z21 : constant Boolean := kgk and sbs;
    z22 : constant Boolean := qgd xor tvj;
    z23 : constant Boolean := grk xor gcj;
    z24 : constant Boolean := kdn xor pbv;
    z25 : constant Boolean := nfm xor vsw;
    z26 : constant Boolean := jkt xor kgs;
    z27 : constant Boolean := qrw xor mjf;
    z28 : constant Boolean := rtb xor whc;
    z29 : constant Boolean := wtw xor vmj;
    z30 : constant Boolean := bqb xor qgc;
    z31 : constant Boolean := mwg xor tkw;
    z32 : constant Boolean := mrg xor wrd;
    z33 : constant Boolean := pmg xor rnc;
    z34 : constant Boolean := bgg xor kvw;
    z35 : constant Boolean := bhd xor prn;
    z36 : constant Boolean := dkj xor dmj;
    z37 : constant Boolean := vjg xor jgc;
    z38 : constant Boolean := spg xor bwb;
    z40 : constant Boolean := qwt xor nhk;
    z41 : constant Boolean := sgk xor krk;
    z42 : constant Boolean := pwt xor sbn;
    z43 : constant Boolean := gdt xor nsb;
    z44 : constant Boolean := tdh xor jrb;
    z45 : constant Boolean := pjg or cvk;

    z : constant Integer_64 :=
      Boolean'Pos (z00) * 2 ** 0 +
      Boolean'Pos (z01) * 2 ** 1 +
      Boolean'Pos (z02) * 2 ** 2 +
      Boolean'Pos (z03) * 2 ** 3 +
      Boolean'Pos (z04) * 2 ** 4 +
      Boolean'Pos (z05) * 2 ** 5 +
      Boolean'Pos (z06) * 2 ** 6 +
      Boolean'Pos (z07) * 2 ** 7 +
      Boolean'Pos (z08) * 2 ** 8 +
      Boolean'Pos (z09) * 2 ** 9 +
      Boolean'Pos (z10) * 2 ** 10 +
      Boolean'Pos (z11) * 2 ** 11 +
      Boolean'Pos (z12) * 2 ** 12 +
      Boolean'Pos (z13) * 2 ** 13 +
      Boolean'Pos (z14) * 2 ** 14 +
      Boolean'Pos (z15) * 2 ** 15 +
      Boolean'Pos (z16) * 2 ** 16 +
      Boolean'Pos (z17) * 2 ** 17 +
      Boolean'Pos (z18) * 2 ** 18 +
      Boolean'Pos (z19) * 2 ** 19 +
      Boolean'Pos (z20) * 2 ** 20 +
      Boolean'Pos (z21) * 2 ** 21 +
      Boolean'Pos (z22) * 2 ** 22 +
      Boolean'Pos (z23) * 2 ** 23 +
      Boolean'Pos (z24) * 2 ** 24 +
      Boolean'Pos (z25) * 2 ** 25 +
      Boolean'Pos (z26) * 2 ** 26 +
      Boolean'Pos (z27) * 2 ** 27 +
      Boolean'Pos (z28) * 2 ** 28 +
      Boolean'Pos (z29) * 2 ** 29 +
      Boolean'Pos (z30) * 2 ** 30 +
      Boolean'Pos (z31) * 2 ** 31 +
      Boolean'Pos (z32) * 2 ** 32 +
      Boolean'Pos (z33) * 2 ** 33 +
      Boolean'Pos (z34) * 2 ** 34 +
      Boolean'Pos (z35) * 2 ** 35 +
      Boolean'Pos (z36) * 2 ** 36 +
      Boolean'Pos (z37) * 2 ** 37 +
      Boolean'Pos (z38) * 2 ** 38 +
      Boolean'Pos (z39) * 2 ** 39 +
      Boolean'Pos (z40) * 2 ** 40 +
      Boolean'Pos (z41) * 2 ** 41 +
      Boolean'Pos (z42) * 2 ** 42 +
      Boolean'Pos (z43) * 2 ** 43 +
      Boolean'Pos (z44) * 2 ** 44 +
      Boolean'Pos (z45) * 2 ** 45;
  begin
    r (part_1) := Trim_Left (+z'Image);
  end Do_Part_1;

  procedure Do_Part_2 is

    function Add (x, y : Integer_64) return Integer_64 is

      x00 : constant Boolean := Boolean'Val ((x / (2 ** 0)) mod 2);
      x01 : constant Boolean := Boolean'Val ((x / (2 ** 1)) mod 2);
      x02 : constant Boolean := Boolean'Val ((x / (2 ** 2)) mod 2);
      x03 : constant Boolean := Boolean'Val ((x / (2 ** 3)) mod 2);
      x04 : constant Boolean := Boolean'Val ((x / (2 ** 4)) mod 2);
      x05 : constant Boolean := Boolean'Val ((x / (2 ** 5)) mod 2);
      x06 : constant Boolean := Boolean'Val ((x / (2 ** 6)) mod 2);
      x07 : constant Boolean := Boolean'Val ((x / (2 ** 7)) mod 2);
      x08 : constant Boolean := Boolean'Val ((x / (2 ** 8)) mod 2);
      x09 : constant Boolean := Boolean'Val ((x / (2 ** 9)) mod 2);
      x10 : constant Boolean := Boolean'Val ((x / (2 ** 10)) mod 2);
      x11 : constant Boolean := Boolean'Val ((x / (2 ** 11)) mod 2);
      x12 : constant Boolean := Boolean'Val ((x / (2 ** 12)) mod 2);
      x13 : constant Boolean := Boolean'Val ((x / (2 ** 13)) mod 2);
      x14 : constant Boolean := Boolean'Val ((x / (2 ** 14)) mod 2);
      x15 : constant Boolean := Boolean'Val ((x / (2 ** 15)) mod 2);
      x16 : constant Boolean := Boolean'Val ((x / (2 ** 16)) mod 2);
      x17 : constant Boolean := Boolean'Val ((x / (2 ** 17)) mod 2);
      x18 : constant Boolean := Boolean'Val ((x / (2 ** 18)) mod 2);
      x19 : constant Boolean := Boolean'Val ((x / (2 ** 19)) mod 2);
      x20 : constant Boolean := Boolean'Val ((x / (2 ** 20)) mod 2);
      x21 : constant Boolean := Boolean'Val ((x / (2 ** 21)) mod 2);
      x22 : constant Boolean := Boolean'Val ((x / (2 ** 22)) mod 2);
      x23 : constant Boolean := Boolean'Val ((x / (2 ** 23)) mod 2);
      x24 : constant Boolean := Boolean'Val ((x / (2 ** 24)) mod 2);
      x25 : constant Boolean := Boolean'Val ((x / (2 ** 25)) mod 2);
      x26 : constant Boolean := Boolean'Val ((x / (2 ** 26)) mod 2);
      x27 : constant Boolean := Boolean'Val ((x / (2 ** 27)) mod 2);
      x28 : constant Boolean := Boolean'Val ((x / (2 ** 28)) mod 2);
      x29 : constant Boolean := Boolean'Val ((x / (2 ** 29)) mod 2);
      x30 : constant Boolean := Boolean'Val ((x / (2 ** 30)) mod 2);
      x31 : constant Boolean := Boolean'Val ((x / (2 ** 31)) mod 2);
      x32 : constant Boolean := Boolean'Val ((x / (2 ** 32)) mod 2);
      x33 : constant Boolean := Boolean'Val ((x / (2 ** 33)) mod 2);
      x34 : constant Boolean := Boolean'Val ((x / (2 ** 34)) mod 2);
      x35 : constant Boolean := Boolean'Val ((x / (2 ** 35)) mod 2);
      x36 : constant Boolean := Boolean'Val ((x / (2 ** 36)) mod 2);
      x37 : constant Boolean := Boolean'Val ((x / (2 ** 37)) mod 2);
      x38 : constant Boolean := Boolean'Val ((x / (2 ** 38)) mod 2);
      x39 : constant Boolean := Boolean'Val ((x / (2 ** 39)) mod 2);
      x40 : constant Boolean := Boolean'Val ((x / (2 ** 40)) mod 2);
      x41 : constant Boolean := Boolean'Val ((x / (2 ** 41)) mod 2);
      x42 : constant Boolean := Boolean'Val ((x / (2 ** 42)) mod 2);
      x43 : constant Boolean := Boolean'Val ((x / (2 ** 43)) mod 2);
      x44 : constant Boolean := Boolean'Val ((x / (2 ** 44)) mod 2);

      y00 : constant Boolean := Boolean'Val ((y / (2 ** 0)) mod 2);
      y01 : constant Boolean := Boolean'Val ((y / (2 ** 1)) mod 2);
      y02 : constant Boolean := Boolean'Val ((y / (2 ** 2)) mod 2);
      y03 : constant Boolean := Boolean'Val ((y / (2 ** 3)) mod 2);
      y04 : constant Boolean := Boolean'Val ((y / (2 ** 4)) mod 2);
      y05 : constant Boolean := Boolean'Val ((y / (2 ** 5)) mod 2);
      y06 : constant Boolean := Boolean'Val ((y / (2 ** 6)) mod 2);
      y07 : constant Boolean := Boolean'Val ((y / (2 ** 7)) mod 2);
      y08 : constant Boolean := Boolean'Val ((y / (2 ** 8)) mod 2);
      y09 : constant Boolean := Boolean'Val ((y / (2 ** 9)) mod 2);
      y10 : constant Boolean := Boolean'Val ((y / (2 ** 10)) mod 2);
      y11 : constant Boolean := Boolean'Val ((y / (2 ** 11)) mod 2);
      y12 : constant Boolean := Boolean'Val ((y / (2 ** 12)) mod 2);
      y13 : constant Boolean := Boolean'Val ((y / (2 ** 13)) mod 2);
      y14 : constant Boolean := Boolean'Val ((y / (2 ** 14)) mod 2);
      y15 : constant Boolean := Boolean'Val ((y / (2 ** 15)) mod 2);
      y16 : constant Boolean := Boolean'Val ((y / (2 ** 16)) mod 2);
      y17 : constant Boolean := Boolean'Val ((y / (2 ** 17)) mod 2);
      y18 : constant Boolean := Boolean'Val ((y / (2 ** 18)) mod 2);
      y19 : constant Boolean := Boolean'Val ((y / (2 ** 19)) mod 2);
      y20 : constant Boolean := Boolean'Val ((y / (2 ** 20)) mod 2);
      y21 : constant Boolean := Boolean'Val ((y / (2 ** 21)) mod 2);
      y22 : constant Boolean := Boolean'Val ((y / (2 ** 22)) mod 2);
      y23 : constant Boolean := Boolean'Val ((y / (2 ** 23)) mod 2);
      y24 : constant Boolean := Boolean'Val ((y / (2 ** 24)) mod 2);
      y25 : constant Boolean := Boolean'Val ((y / (2 ** 25)) mod 2);
      y26 : constant Boolean := Boolean'Val ((y / (2 ** 26)) mod 2);
      y27 : constant Boolean := Boolean'Val ((y / (2 ** 27)) mod 2);
      y28 : constant Boolean := Boolean'Val ((y / (2 ** 28)) mod 2);
      y29 : constant Boolean := Boolean'Val ((y / (2 ** 29)) mod 2);
      y30 : constant Boolean := Boolean'Val ((y / (2 ** 30)) mod 2);
      y31 : constant Boolean := Boolean'Val ((y / (2 ** 31)) mod 2);
      y32 : constant Boolean := Boolean'Val ((y / (2 ** 32)) mod 2);
      y33 : constant Boolean := Boolean'Val ((y / (2 ** 33)) mod 2);
      y34 : constant Boolean := Boolean'Val ((y / (2 ** 34)) mod 2);
      y35 : constant Boolean := Boolean'Val ((y / (2 ** 35)) mod 2);
      y36 : constant Boolean := Boolean'Val ((y / (2 ** 36)) mod 2);
      y37 : constant Boolean := Boolean'Val ((y / (2 ** 37)) mod 2);
      y38 : constant Boolean := Boolean'Val ((y / (2 ** 38)) mod 2);
      y39 : constant Boolean := Boolean'Val ((y / (2 ** 39)) mod 2);
      y40 : constant Boolean := Boolean'Val ((y / (2 ** 40)) mod 2);
      y41 : constant Boolean := Boolean'Val ((y / (2 ** 41)) mod 2);
      y42 : constant Boolean := Boolean'Val ((y / (2 ** 42)) mod 2);
      y43 : constant Boolean := Boolean'Val ((y / (2 ** 43)) mod 2);
      y44 : constant Boolean := Boolean'Val ((y / (2 ** 44)) mod 2);

      --  Half-Adder 1: Carry

      cvk : constant Boolean := y44 and x44;
      bmf : constant Boolean := x43 and y43;
      qjp : constant Boolean := y42 and x42;
      kfp : constant Boolean := y41 and x41;
      c40_1_qhf : constant Boolean := y40 and x40;

      c38_1_crp : constant Boolean := x38 and y38;
      c37_1_nbc : constant Boolean := y37 and x37;
      pfc : constant Boolean := x36 and y36;
      qph : constant Boolean := y35 and x35;
      rkq : constant Boolean := y34 and x34;
      hbn : constant Boolean := x33 and y33;
      ttt : constant Boolean := x32 and y32;
      gpp : constant Boolean := y31 and x31;
      rvc : constant Boolean := y30 and x30;
      bqn : constant Boolean := x29 and y29;
      shp : constant Boolean := x28 and y28;
      fjr : constant Boolean := y27 and x27;
      jck : constant Boolean := x26 and y26;
      dbt : constant Boolean := y25 and x25;
      frv : constant Boolean := x24 and y24;
      nrw : constant Boolean := y23 and x23;
      dpg : constant Boolean := x22 and y22;
      c21_1_knb : constant Boolean := x21 and y21;
      c20_1_vwh : constant Boolean := y20 and x20;
      hhb : constant Boolean := x19 and y19;
      gpq : constant Boolean := y18 and x18;
      fpk : constant Boolean := x17 and y17;
      fhn : constant Boolean := y16 and x16;
      vch : constant Boolean := y15 and x15;
      ktg : constant Boolean := x14 and y14;
      c13_1_wbw : constant Boolean := y13 and x13;
      c12_1_njd : constant Boolean := x12 and y12;
      c11_1_cbm : constant Boolean := x11 and y11;
      c10_1_bcw : constant Boolean := y10 and x10;
      c09_1_ptc : constant Boolean := x09 and y09;
      c08_1_qpg : constant Boolean := y08 and x08;
      c07_1_vjc : constant Boolean := y07 and x07;
      knd : constant Boolean := y06 and x06;
      fkt : constant Boolean := x05 and y05;
      dtn : constant Boolean := y04 and x04;
      rtt : constant Boolean := y03 and x03;
      tng : constant Boolean := y02 and x02;
      hnj : constant Boolean := x01 and y01;
      jcq : constant Boolean := x00 and y00;

      --  Half-Adder 1: Sum

      jrb : constant Boolean := x44 xor y44;
      nsb : constant Boolean := x43 xor y43;
      sbn : constant Boolean := x42 xor y42;
      krk : constant Boolean := x41 xor y41;
      s40_1_qwt : constant Boolean := x40 xor y40;
      s39_1_ksf : constant Boolean := x39 xor y39;
      s38_1_spg : constant Boolean := x38 xor y38;
      s37_1_vjg : constant Boolean := x37 xor y37;
      s36_1_dkj : constant Boolean := x36 xor y36;
      prn : constant Boolean := y35 xor x35;
      bgg : constant Boolean := x34 xor y34;
      pmg : constant Boolean := y33 xor x33;
      wrd : constant Boolean := y32 xor x32;
      tkw : constant Boolean := y31 xor x31;
      qgc : constant Boolean := y30 xor x30;
      wtw : constant Boolean := x29 xor y29;
      rtb : constant Boolean := x28 xor y28;
      qrw : constant Boolean := y27 xor x27;
      jkt : constant Boolean := y26 xor x26;
      nfm : constant Boolean := x25 xor y25;
      kdn : constant Boolean := y24 xor x24;
      gcj : constant Boolean := x23 xor y23;
      qgd : constant Boolean := x22 xor y22;
      s21_1_sbs : constant Boolean := y21 xor x21;
      s20_1_vdc : constant Boolean := x20 xor y20;
      mcv : constant Boolean := x19 xor y19;
      jbp : constant Boolean := y18 xor x18;
      tdb : constant Boolean := x17 xor y17;
      vgb : constant Boolean := x16 xor y16;
      pvj : constant Boolean := y15 xor x15;
      c14_1_tfh : constant Boolean := y14 xor x14;
      s13_1_wgb : constant Boolean := y13 xor x13;
      s12_1_htr : constant Boolean := x12 xor y12;
      s11_1_ksb : constant Boolean := x11 xor y11;
      s10_1_cvv : constant Boolean := x10 xor y10;
      s09_1_mnm : constant Boolean := x09 xor y09;
      s08_1_jhr : constant Boolean := x08 xor y08;
      s07_1_gjv : constant Boolean := y07 xor x07;
      hsn : constant Boolean := x06 xor y06;
      sfn : constant Boolean := y05 xor x05;
      ptv : constant Boolean := y04 xor x04;
      npk : constant Boolean := x03 xor y03;
      qpd : constant Boolean := y02 xor x02;
      sqk : constant Boolean := x01 xor y01;

      z00 : constant Boolean := x00 xor y00;  --  No carry in.

      bmc : constant Boolean := sqk and jcq;
      kpf : constant Boolean := hnj or bmc;
      pwk : constant Boolean := kpf and qpd;
      tpv : constant Boolean := tng or pwk;
      hrr : constant Boolean := tpv and npk;

      bdk : constant Boolean := hrr or rtt;

      nnq : constant Boolean := ptv and bdk;
      svk : constant Boolean := dtn or nnq;
      wnq : constant Boolean := sfn and svk;
      cjh : constant Boolean := fkt or wnq;
      hds : constant Boolean := cjh and hsn;
      wcs : constant Boolean := knd or hds;
      nbb : constant Boolean := s07_1_gjv and wcs;
      c08_bwm   : constant Boolean := nbb or c07_1_vjc;
      c08_2_tvw : constant Boolean := c08_bwm and s08_1_jhr;
      c09_gqb   : constant Boolean := c08_2_tvw or c08_1_qpg;
      c09_2_dsk : constant Boolean := c09_gqb and s09_1_mnm;

      z09 : constant Boolean := s09_1_mnm xor c09_gqb;
      c10_gwh : constant Boolean := c09_2_dsk or c09_1_ptc;

      c10_2_pgn : constant Boolean := c10_gwh and s10_1_cvv;
      c11_jbk   : constant Boolean := c10_1_bcw or c10_2_pgn;
      c11_2_hgb : constant Boolean := s11_1_ksb and c11_jbk;

      c12_jgf : constant Boolean := c11_1_cbm or c11_2_hgb;
      c12_2_dmc  : constant Boolean := c12_jgf and s12_1_htr;

      c13_qwr : constant Boolean := c12_2_dmc or c12_1_njd;

      c13_2_dqm : constant Boolean := c13_1_wbw and c13_qwr;  --  !!

      nkd : constant Boolean := c13_2_dqm or s13_1_wgb;  --  c14?

      jrd : constant Boolean := c14_1_tfh and nkd;
      bgh : constant Boolean := jrd or ktg;

      vcd : constant Boolean := bgh and pvj;
      nhh : constant Boolean := vcd or vch;
      btr : constant Boolean := vgb and nhh;
      ctv : constant Boolean := btr or fhn;
      ffh : constant Boolean := tdb and ctv;
      wtc : constant Boolean := fpk or ffh;
      pfh : constant Boolean := jbp and wtc;
      ppf : constant Boolean := pfh or gpq;
      gvt : constant Boolean := ppf and mcv;
      c20_tjc : constant Boolean := gvt or hhb;
      c20_2_vvc : constant Boolean := s20_1_vdc and c20_tjc;
      c21_kgk : constant Boolean := c20_1_vwh or c20_2_vvc;

      z21 : constant Boolean := c21_kgk xor s21_1_sbs;
      c21_2_rcb : constant Boolean := c21_kgk and s21_1_sbs;

      tvj : constant Boolean := c21_2_rcb or c21_1_knb;
      vsb : constant Boolean := qgd and tvj;
      grk : constant Boolean := dpg or vsb;
      cdw : constant Boolean := gcj and grk;
      pbv : constant Boolean := nrw or cdw;
      mhd : constant Boolean := kdn and pbv;
      vsw : constant Boolean := frv or mhd;
      dfq : constant Boolean := nfm and vsw;
      kgs : constant Boolean := dfq or dbt;
      wtd : constant Boolean := kgs and jkt;
      mjf : constant Boolean := jck or wtd;
      jgh : constant Boolean := mjf and qrw;
      whc : constant Boolean := fjr or jgh;
      jsf : constant Boolean := rtb and whc;
      vmj : constant Boolean := jsf or shp;
      nbw : constant Boolean := vmj and wtw;
      bqb : constant Boolean := nbw or bqn;
      nth : constant Boolean := qgc and bqb;
      mwg : constant Boolean := nth or rvc;
      vmq : constant Boolean := tkw and mwg;
      mrg : constant Boolean := gpp or vmq;
      vcm : constant Boolean := mrg and wrd;
      rnc : constant Boolean := vcm or ttt;
      dmp : constant Boolean := pmg and rnc;
      kvw : constant Boolean := hbn or dmp;
      gwm : constant Boolean := kvw and bgg;
      bhd : constant Boolean := rkq or gwm;

      drv : constant Boolean := prn and bhd;
      dmj : constant Boolean := drv or qph;
      vnw : constant Boolean := dmj and s36_1_dkj;
      jgc : constant Boolean := pfc or vnw;
      mqh : constant Boolean := s37_1_vjg and jgc;
      bwb : constant Boolean := c37_1_nbc or mqh;

      c38_2_wjg : constant Boolean := s38_1_spg and bwb;
      c39_wjf : constant Boolean := c38_1_crp or c38_2_wjg;
      c39_2_hvf : constant Boolean := s39_1_ksf and c39_wjf;

      z39_ok : constant Boolean := c39_wjf xor s39_1_ksf;
      c39_1_jct : constant Boolean := x39 and y39;

      nhk : constant Boolean := c39_1_jct or c39_2_hvf;
      qwh : constant Boolean := nhk and s40_1_qwt;
      sgk : constant Boolean := c40_1_qhf or qwh;
      qvv : constant Boolean := sgk and krk;
      pwt : constant Boolean := kfp or qvv;
      qwg : constant Boolean := sbn and pwt;
      gdt : constant Boolean := qwg or qjp;
      dnc : constant Boolean := nsb and gdt;

      tdh : constant Boolean := dnc or bmf;
      pjg : constant Boolean := jrb and tdh;

      --  Half-Adder 2: Sum

      z01 : constant Boolean := jcq xor sqk;
      z02 : constant Boolean := kpf xor qpd;
      z03 : constant Boolean := tpv xor npk;
      z04 : constant Boolean := ptv xor bdk;
      z05 : constant Boolean := svk xor sfn;
      z06 : constant Boolean := cjh xor hsn;
      z07 : constant Boolean := wcs xor s07_1_gjv;
      z08 : constant Boolean := c08_bwm xor s08_1_jhr;

      z10 : constant Boolean := s10_1_cvv xor c10_gwh;
      z11 : constant Boolean := s11_1_ksb xor c11_jbk;
      z12 : constant Boolean := s12_1_htr xor c12_jgf;
      z13 : constant Boolean := c13_qwr xor c13_1_wbw;  --  should be c13 xor s13_1
      z14 : constant Boolean := nkd xor c14_1_tfh;
      z15 : constant Boolean := bgh xor pvj;
      z16 : constant Boolean := nhh xor vgb;
      z17 : constant Boolean := ctv xor tdb;
      z18 : constant Boolean := wtc xor jbp;
      z19 : constant Boolean := mcv xor ppf;
      z20 : constant Boolean := s20_1_vdc xor c20_tjc;
      z22 : constant Boolean := qgd xor tvj;
      z23 : constant Boolean := grk xor gcj;
      z24 : constant Boolean := kdn xor pbv;
      z25 : constant Boolean := nfm xor vsw;
      z26 : constant Boolean := jkt xor kgs;
      z27 : constant Boolean := qrw xor mjf;
      z28 : constant Boolean := rtb xor whc;
      z29 : constant Boolean := wtw xor vmj;
      z30 : constant Boolean := bqb xor qgc;
      z31 : constant Boolean := mwg xor tkw;
      z32 : constant Boolean := mrg xor wrd;
      z33 : constant Boolean := pmg xor rnc;
      z34 : constant Boolean := bgg xor kvw;
      z35 : constant Boolean := bhd xor prn;
      z36 : constant Boolean := s36_1_dkj xor dmj;
      z37 : constant Boolean := s37_1_vjg xor jgc;
      z38 : constant Boolean := s38_1_spg xor bwb;
      z40 : constant Boolean := s40_1_qwt xor nhk;
      z41 : constant Boolean := sgk xor krk;
      z42 : constant Boolean := pwt xor sbn;
      z43 : constant Boolean := gdt xor nsb;
      z44 : constant Boolean := tdh xor jrb;
      z45 : constant Boolean := pjg or  cvk;  --  Last carry

      z : constant Integer_64 :=
        Boolean'Pos (z00) * 2 ** 0 +
        Boolean'Pos (z01) * 2 ** 1 +
        Boolean'Pos (z02) * 2 ** 2 +
        Boolean'Pos (z03) * 2 ** 3 +
        Boolean'Pos (z04) * 2 ** 4 +
        Boolean'Pos (z05) * 2 ** 5 +
        Boolean'Pos (z06) * 2 ** 6 +
        Boolean'Pos (z07) * 2 ** 7 +
        Boolean'Pos (z08) * 2 ** 8 +
        Boolean'Pos (z09) * 2 ** 9 +
        Boolean'Pos (z10) * 2 ** 10 +
        Boolean'Pos (z11) * 2 ** 11 +
        Boolean'Pos (z12) * 2 ** 12 +
        Boolean'Pos (z13) * 2 ** 13 +
        Boolean'Pos (z14) * 2 ** 14 +
        Boolean'Pos (z15) * 2 ** 15 +
        Boolean'Pos (z16) * 2 ** 16 +
        Boolean'Pos (z17) * 2 ** 17 +
        Boolean'Pos (z18) * 2 ** 18 +
        Boolean'Pos (z19) * 2 ** 19 +
        Boolean'Pos (z20) * 2 ** 20 +
        Boolean'Pos (z21) * 2 ** 21 +
        Boolean'Pos (z22) * 2 ** 22 +
        Boolean'Pos (z23) * 2 ** 23 +
        Boolean'Pos (z24) * 2 ** 24 +
        Boolean'Pos (z25) * 2 ** 25 +
        Boolean'Pos (z26) * 2 ** 26 +
        Boolean'Pos (z27) * 2 ** 27 +
        Boolean'Pos (z28) * 2 ** 28 +
        Boolean'Pos (z29) * 2 ** 29 +
        Boolean'Pos (z30) * 2 ** 30 +
        Boolean'Pos (z31) * 2 ** 31 +
        Boolean'Pos (z32) * 2 ** 32 +
        Boolean'Pos (z33) * 2 ** 33 +
        Boolean'Pos (z34) * 2 ** 34 +
        Boolean'Pos (z35) * 2 ** 35 +
        Boolean'Pos (z36) * 2 ** 36 +
        Boolean'Pos (z37) * 2 ** 37 +
        Boolean'Pos (z38) * 2 ** 38 +
        Boolean'Pos (z39_ok) * 2 ** 39 +
        Boolean'Pos (z40) * 2 ** 40 +
        Boolean'Pos (z41) * 2 ** 41 +
        Boolean'Pos (z42) * 2 ** 42 +
        Boolean'Pos (z43) * 2 ** 43 +
        Boolean'Pos (z44) * 2 ** 44 +
        Boolean'Pos (z45) * 2 ** 45;
    begin
      return z;
    end Add;

    function Check (x, y : Integer_64) return Boolean is
    begin
      return Add (x, y) = x + y;
    end Check;

  begin
    for i in 0 .. 44 loop
      for j in 0 .. 44 loop
        if not Check (2**i, 2**j) then
          Put_Line (i'Image & ", " & j'Image);
        end if;
      end loop;
    end loop;
    r (part_2) := +"gwh,jct,rcb,wbw,wgb,z09,z21,z39";
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  --  verbose : constant Boolean := True;
  T0 : constant Time := Clock;

begin
  Do_Part_1;
  if not compiler_test_mode then
    Do_Part_2;
  end if;

  if compiler_test_mode then
    if r (part_1) /= Argument (1) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 57270694330992
    --  Part 2: validated by AoC: gwh,jct,rcb,wbw,wgb,z09,z21,z39
  end if;
end AoC_2024_24;
