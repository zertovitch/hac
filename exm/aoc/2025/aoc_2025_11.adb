--  Solution to Advent of Code 2025, Day 11
-------------------------------------------
--  Reactor
--
--  https://adventofcode.com/2025/day/11
--  Copy of questions in: aoc_2025_11_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in the upper directory: ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2025.gpr .
with HAT;

with Interfaces;

procedure AoC_2025_11 is

  use AoC_Toolbox, HAT, Interfaces;

  input_name : constant VString := +"aoc_2025_11";

  r : array (Part_Type) of VString;

  type Device is
  (you,
   imt,
   grh,
   yae,
   luf,
   ypt,
   rlf,
   xls,
   ggr,
   xgr,
   mze,
   xfb,
   wms,
   lpm,
   wov,
   oop,
   cnb,
   xaz,
   svr,
   vya,
   zuk,
   gvw,
   ztu,
   jmg,
   hxf,
   dsq,
   jpr,
   qba,
   zyt,
   wke,
   hbz,
   xru,
   wsw,
   ifh,
   xpt,
   elr,
   bbg,
   bye,
   ceb,
   msa,
   gzq,
   ufd,
   jev,
   gix,
   see,
   rnu,
   axr,
   efu,
   cnf,
   vzq,
   ehj,
   xup,
   tak,
   omn,
   kmj,
   hpe,
   xzx,
   qiu,
   sar,
   wur,
   zvs,
   bez,
   esf,
   taz,
   fgw,
   wqf,
   zab,
   ars,
   pmn,
   lay,
   imd,
   rak,
   ixj,
   pka,
   wrm,
   bik,
   slz,
   wxx,
   hju,
   dcs,
   yuo,
   ucg,
   vtp,
   znh,
   slu,
   qeh,
   ing,
   ogh,
   mlo,
   nhx,
   wrf,
   ihw,
   yut,
   drp,
   ywc,
   bsl,
   uhu,
   mcp,
   lce,
   vax,
   fdo,
   lmn,
   ebx,
   cdt,
   oka,
   zny,
   bxj,
   azn,
   aps,
   pvr,
   kxq,
   fry,
   tth,
   ize,
   vfi,
   qtx,
   qck,
   lft,
   iur,
   dqs,
   xci,
   ayn,
   frn,
   fhh,
   jkp,
   fka,
   lcc,
   yrq,
   zaw,
   ggb,
   vkw,
   nmm,
   kak,
   ejn,
   yba,
   hlb,
   lka,
   gfk,
   arh,
   gtw,
   imk,
   mox,
   fuu,
   nfe,
   bdv,
   asn,
   yas,
   cyr,
   wdt,
   mef,
   ihs,
   box,
   sqj,
   olv,
   hsh,
   ifc,
   nko,
   oim,
   fwk,
   djs,
   pew,
   vka,
   mbw,
   mgz,
   lvp,
   bxp,
   lgw,
   mbu,
   jdx,
   boq,
   ltz,
   com,
   fll,
   obk,
   vht,
   zyh,
   hni,
   vsy,
   kfc,
   mka,
   uoh,
   byu,
   pwf,
   rxh,
   zxf,
   lvi,
   mfj,
   one,
   qsj,
   gko,
   boe,
   pum,
   tvs,
   irl,
   bqb,
   qsl,
   kyx,
   the,
   fob,
   nfq,
   ams,
   cwr,
   gia,
   fmh,
   ndx,
   fpc,
   ysn,
   cwa,
   lfq,
   wth,
   hjx,
   vkn,
   afp,
   zgd,
   fph,
   riq,
   ajh,
   qvo,
   vzk,
   wld,
   xym,
   akx,
   mmq,
   nft,
   ppl,
   dkm,
   wgp,
   vlb,
   gou,
   vwe,
   qpu,
   rho,
   han,
   swp,
   wji,
   sef,
   jje,
   xly,
   lkw,
   tvg,
   tat,
   fft,
   tym,
   cqz,
   xbu,
   enr,
   sjd,
   piz,
   kgv,
   cwm,
   ogu,
   cxx,
   qfb,
   ptb,
   pui,
   zkr,
   pif,
   oaw,
   qjd,
   pcx,
   pwt,
   szq,
   lfc,
   haj,
   etc,
   jdz,
   sjq,
   cuq,
   qun,
   pny,
   fwa,
   mgf,
   uxl,
   vzo,
   alf,
   xvf,
   dyq,
   yek,
   wcn,
   xcd,
   jcv,
   gqb,
   rab,
   srf,
   kme,
   ahr,
   ljd,
   kmd,
   swz,
   qnt,
   pqj,
   bio,
   pbv,
   gip,
   dus,
   vzx,
   tii,
   sve,
   fcg,
   irm,
   osd,
   ftn,
   xpf,
   dlh,
   boc,
   lke,
   fbf,
   zuj,
   kwq,
   dac,
   vcd,
   cju,
   ayb,
   jna,
   hfg,
   ajl,
   dxs,
   vwv,
   iwf,
   mcy,
   qhv,
   ove,
   gdl,
   fhl,
   oiu,
   fbj,
   lff,
   rxs,
   hxe,
   bkg,
   jaf,
   xhd,
   yor,
   zpc,
   ovj,
   ixl,
   hlr,
   gbv,
   kdd,
   osb,
   tfa,
   qvu,
   jdy,
   nkv,
   ojn,
   rfy,
   xmm,
   jzt,
   mio,
   frk,
   kqf,
   kft,
   pqr,
   kcx,
   ytn,
   wau,
   ljj,
   jzg,
   ryr,
   xcb,
   uco,
   pqf,
   mgb,
   diw,
   aak,
   vha,
   pgu,
   phe,
   zir,
   vbv,
   fwb,
   ipq,
   xlv,
   aai,
   qnf,
   sym,
   ppg,
   nak,
   vqp,
   hca,
   tla,
   xmh,
   brj,
   ohg,
   mck,
   bne,
   uug,
   wuh,
   how,
   glc,
   isf,
   elg,
   mgq,
   upa,
   agt,
   kot,
   ftb,
   uzi,
   brh,
   aev,
   ats,
   eae,
   foi,
   qdl,
   ngp,
   yir,
   cfh,
   asf,
   vwl,
   lyf,
   hlc,
   etm,
   zrk,
   fse,
   rus,
   nei,
   izt,
   ptn,
   ebd,
   rcr,
   txb,
   lkh,
   zal,
   wpb,
   ooc,
   wjd,
   ona,
   qqj,
   zco,
   izi,
   hpj,
   rpb,
   miw,
   qte,
   wpu,
   awd,
   byh,
   fac,
   rbx,
   dwc,
   wbn,
   hly,
   sdj,
   une,
   ujn,
   ouy,
   swg,
   amb,
   dap,
   nnz,
   xfk,
   nik,
   nmz,
   bic,
   enh,
   pcm,
   qnw,
   dhn,
   dqx,
   skx,
   mpw,
   wyd,
   nso,
   mjd,
   cmu,
   jdo,
   cxk,
   gua,
   ewp,
   fzl,
   gkv,
   cnt,
   bja,
   kbq,
   ums,
   mjn,
   qdj,
   ulf,
   gcc,
   sjy,
   kow,
   xjh,
   ogt,
   znt,
   jox,
   bhg,
   xnn,
   wzm,
   pgr,
   kwb,
   elm,
   tei,
   gvy,
   ryk,
   dkw,
   epu,
   vwt,
   hwr,
   jjf,
   cir,
   yxm,
   jvd,
   byz,
   rrt,
   vjs,
   wax,
   xdb,
   ozj,
   bao,
   pue,
   fgc,
   hkw,
   xob,
   smv,
   vnu,
   gir,
   hou,
   lne,
   sfj,
   qmn,
   otd,
   qfo,
   ujb,
   ped,
   dcm,
   hai,
   vns,
   kjs,
   aso,
   rpp,
   fjw,
   ezl,
   huk,
   mzm,
   zzp,
   ahf,
   auy,
   xva,
   wci,
   bhk,
   tur,
   zvu,
   nkt,
   pdz,
   fnj,
   tco,
   jix,
   cwe,
   jmd,
   cji,
   aod,
   kdb,
   xos,
   --  Example:
   aaa,
   bbb,
   ccc,
   ddd,
   eee,
   fff,
   ggg,
   hhh,
   iii,
   --  Other example:
   tty,
   hub,
   --
   tuo);  --  Out

  connection : array (Device, 1 .. 50) of Device;
  connections, paths : array (Device) of Natural;

  procedure Read_Data is
    unused_separator : Character;
    asm : String (1 .. 3);
    i : Integer;
    f : File_Type;
    s : VString;
    row : Device;
  begin
    for d in Device loop
      connections (d) := 0;
    end loop;
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get (f, asm);
      row := Device'Value (asm);
      Get (f, unused_separator);
      --  put (row'image & ':');
      i := 0;
      Get_Line (f, s);
      loop
        Delete (s, 1, 1);  --  ' '
        i := i + 1;
        connection (row, i) := Device'Value (To_String (Slice (s, 1, 3)));
        --  put (i, 0); put (" " & connection(row,i)'Image);
        Delete (s, 1, 3);
        exit when Length (s) = 0;
      end loop;
      connections (row) := i;
      --  new_line;
    end loop;
    Close (f);
  end Read_Data;

  procedure Reset_Paths is
  begin
    for d in Device loop
      paths (d) := Integer'Last;
    end loop;
  end Reset_Paths;

  function Count_Paths (from, to : Device) return Natural is
    res : Natural;
  begin
    if from = to then
      return 1;
    elsif from = tuo then
      --  Put_Line ("OUT");
      return 0;
    else
      --  Memoization, get:
      if paths (from) < Integer'Last then
        return paths (from);
      else
        res := 0;
        for i in 1 .. connections (from) loop
          --  Put (+"From: " & from'Image & ", Node: " & i & "/" & connections (from) 'image & ": " & connection (from, i)'Image);
          --  Skip_Line;
          res := res + Count_Paths (connection (from, i), to);
        end loop;
        --  Memoization, set:
        paths (from) := res;
        return res;
      end if;
    end if;
  end Count_Paths;

  procedure Do_Part_1 is
  begin
    Reset_Paths;
    r (part_1) := +"" & Count_Paths (you, tuo);
  end Do_Part_1;

  procedure Do_Part_2 is
    res : Integer;
  begin
    Reset_Paths;
    res := Count_Paths (svr, fft);
    Reset_Paths;
    res := res * Count_Paths (fft, dac);
    Reset_Paths;
    res := res * Count_Paths (dac, tuo);
    r (part_2) := +"" & res;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 423.
    --  Part 2: validated by AoC: 333657640517376.
  end if;
end AoC_2025_11;
