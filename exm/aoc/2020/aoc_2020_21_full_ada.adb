--  Solution to Advent of Code 2020, Day 21
-------------------------------------------
--  Allergen Assessment
--
--  https://adventofcode.com/2020/day/21
--
--  Full Ada version.
--
--  Main obstacles for using HAC:
--    - lack of 'Value (or at least 'Image) attribute
--    - lack of logical operators on arrays of Boolean
--
with HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

procedure AoC_2020_21_full_Ada is

  --  Enumerated types created by removing duplicates from data.
  --  See: AoC_2020_21_full_Ada_Preproc.

  type Ingredient is (bfnnnrn, bggmsj, bjcvpc, bkzbrm, blvfz, bmqzgvr,
    bvcxfp, bvlb, bvpxc, bxqpgx, cgflg, chgjqc, cldgd, cncpbssj, cqqcd,
    cvbc, czkfv, czkxlg, czmml, czvphx, dchvb, dfcgd, dgxnc, dhsssf, djdhrn,
    djhj, dkg, drtdz, dxdrk, dxrsgs, fctmvrs, fgcd, fgptl, fgsr, flndv,
    fnlk, fppgp, fqqcnm, fqtc, frxmq, fslqkg, fzlrf, gbhjv, gcpks, gcptp,
    ggrc, gpjr, gpqgkt, gqnxlr, gvdstsc, hbbgk, hbdv, hbmvpmt, hcjkd, hddd,
    hdtr, hfzqf, hjvzcp, hjzkg, hpbnj, hqd, hqzcncv, hrnvd, hzmk, jbtlfv,
    jbz, jfrlp, jgbk, jhbnm, jhv, jlrt, jp, jqgm, jqhn, jqzklv, jsh, jvhsj,
    jxvx, kdbxxzv, kdm, kjjst, kmlkx, ksrjn, ktnlk, lcrs, ldrgj, lftqn,
    lkgln, llj, lpm, lpvfv, lsgqf, lxldx, lzvh, mbmtz, mcl, mgv, mhrlx,
    mkxbf, mrczqmj, mxc, mxq, mxzb, mzbgp, mzrx, nbgv, nfck, nfgr, nggbtk,
    nhnd, nhzthvn, njdb, njqrhcc, njrcfg, nkzqj, nlqhlsn, nmps, nnskqnmn,
    npftghk, nppxr, nqvnn, nthsf, nvmm, nxrgp, pcslhrg, pdhlzg, pdj, pdt,
    pfrqf, phc, pmhhqrk, prbk, prksl, psqc, qdkk, qfslcb, qhtvsr, qjhq,
    qlkfvk, qpbl, qrpzt, qzjrtl, rbjmdn, rdjdq, rdkrtr, rfgf, rftbr, rfx,
    rfxgl, rjc, rlgr, rms, rng, rntk, rsr, rtmfg, rtpff, rtq, rxd, rzcps,
    sfkcp, sjpzc, smdlg, snzxr, spnd, sqhvzg, ssdszsn, ssrgt, strpjp, sxsxm,
    tcclbr, th, thtlt, tkhzz, tktj, tqkfx, tzdks, vbkb, vhjgg, vhjpjdr,
    vnbfvkp, vnjxs, vxmrk, xbvrx, xdcp, xfn, xgpdnz, xqgb, xsk, xtgjslz,
    xvjk, zbxj, zcphr, zdthsvl, zfnttf, zgvtn, zjbgp, zmsdzh, zppxp, zvq);

  type Allergen is (dairy, eggs, fish, nuts, peanuts, sesame, soy, wheat);

  use HAT;

  name : constant VString := +"aoc_2020_21.txt";

  --  type Ingredient is (fvjkl, kfcds, mxmxvkd, nhms, sbzzf, sqjhc, trh);
  --  type Allergen is (dairy, fish, soy);
  --  name : constant VString := +"mini.txt";

  type Ingredient_Set is array (Ingredient) of Boolean;
  type Allergen_Set is array (Allergen) of Boolean;

  type Food is record
    ingr : Ingredient_Set;
    allr : Allergen_Set;
  end record;

  food_list_max : constant := 100;
  foods : Natural := 0;

  type Food_List_Type is array (1 .. food_list_max) of Food;

  food_list : Food_List_Type;

  verbose : constant Boolean := True;

  generic
    type Enum is (<>);
    type Enum_Set is array (Enum) of Boolean;
  procedure Gen_Count (set : Enum_Set; total : out Natural; last : out Enum; verb : Boolean);

  procedure Gen_Count (set : Enum_Set; total : out Natural; last : out Enum; verb : Boolean) is
  begin
    total := 0;
    for e in set'Range loop
      if set (e) then
        if verb then Put (" " & e'Image); end if;
        last := e;
        total := total + 1;
      end if;
    end loop;
    if verb then Put (total'Image & " "); end if;
  end Gen_Count;

  procedure Count is new Gen_Count (Ingredient, Ingredient_Set);

  i_of_a : array (Allergen) of Ingredient;

  procedure Find_Allergens_Ingredients (total_1 : out Natural) is
    --  In a food ingrendient list, not all allergen are listed.
    --  But when an allergen is listed, its associated ingredient IS listed, among others.
    i_for_a : array (Allergen) of Ingredient_Set := (others => (others => True));
    --  An allergen is done when the set of possible ingredients is reduced to 1.
    done : array (Allergen) of Boolean := (others => False);
    total : Natural;
    i : Ingredient;
    scrap : Food_List_Type := food_list;
    to_solve : Natural := Allergen'Pos (Allergen'Last) + 1;
  begin
    loop
      for f in 1 .. foods loop
        for a in Allergen loop
          if scrap (f).allr (a) then
            --  Allergen a is in the food #f.
            --  Then we can narrow the set of possible ingredients for a.
            i_for_a (a) := i_for_a (a) and scrap (f).ingr;
          end if;
        end loop;
      end loop;
      --
      for a in Allergen loop
        if not done (a) then
          if verbose then
            Put ("   " & a'Image & " can only be in ingredients: ");
          end if;
          Count (i_for_a (a), total, i, verbose);
          if verbose then
            New_Line;
          end if;
          if total = 1 then
            --  Hurra, allergen a is ONLY in ONE ingredient, i.
            --  We apply now the rule "Each allergen is found in exactly ONE ingredient."
            for f in 1 .. foods loop
              --  Remove allergenic a and ingredient i everywhere.
              scrap (f).allr (a) := False;
              scrap (f).ingr (i) := False;
            end loop;
            if verbose then
              Put_Line (" -> allergen " & a'Image & " associated to ingredient " & i'Image);
            end if;
            to_solve := to_solve - 1;
            done (a) := True;
            i_of_a (a) := i;
          end if;
        end if;
      end loop;
      exit when to_solve = 0;
    end loop;
    --
    total_1 := 0;
    for f in 1 .. foods loop
      for i in Ingredient loop
        if scrap (f).ingr (i) then  --  Unassociated ingredient.
          total_1 := total_1 + 1;
        end if;
      end loop;
    end loop;
  end Find_Allergens_Ingredients;

  procedure Read_Data is
    f : File_Type;
    s, s1, s2, key : VString;
    i, paren : Integer;
  begin
    Open (f, name);
    while not End_Of_File (f) loop
      foods := foods + 1;
      food_list (foods).ingr := (others => False);
      food_list (foods).allr := (others => False);
      Get_Line (f, s);
      paren := Index (s, "(contains ");
      s1 := Slice (s, 1, paren - 1);
      s2 := Slice (s, paren + 10, Length (s));
      loop
        i := Index (s1, " ");
        exit when i = 0;
        key := Slice (s1, 1, i - 1);
        food_list (foods).ingr (Ingredient'Value (VStr_Pkg.To_String (key))) := True;
        s1 := Slice (s1, i + 1, Length (s1));
      end loop;
      loop
        i := Index (s2, ", ");
        exit when i = 0;
        key := Slice (s2, 1, i - 1);
        food_list (foods).allr (Allergen'Value (VStr_Pkg.To_String (key))) := True;
        s2 := Slice (s2, i + 2, Length (s2));
      end loop;
      key := Slice (s2, 1, Length (s2) - 1);
      food_list (foods).allr (Allergen'Value (VStr_Pkg.To_String (key))) := True;
    end loop;
    Close (f);
  end Read_Data;

  total_n : Natural;

begin
  Read_Data;
  Find_Allergens_Ingredients (total_n);
  Put_Line (+"Part 1: ingredients without known allergens: " & total_n);
  Put ("Part 2: list of ingredients sorted by allergen's name: ");
  for i of i_of_a loop Put (To_Lower (+i'Image) & ','); end loop;
  New_Line;
end AoC_2020_21_full_Ada;
