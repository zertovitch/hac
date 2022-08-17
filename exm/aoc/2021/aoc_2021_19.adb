--  Solution to Advent of Code 2021, Day 19
-------------------------------------------
--  Beacon Scanner
--
--  https://adventofcode.com/2021/day/19
--  Copy of questions in: aoc_2021_19_questions.txt
--
--  Note: this programs takes very long on HAC!
--
--  HAC 0.098 "nice to have"'s detected in this exercise:
--
--    *     Exiting multiple nested loops
--    *     `EXIT [Label]`
--    *     Performance: large "in" parameters passed as reference
--    *     Comparison (equality operators) "=", "/=" of composite types (arrays and records)
--    *     Detect an expression as a static (compile-time-known) value
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_19 is

  type Vector is array (1 .. 3) of Integer;

  subtype Cube_Face is Integer range 1 .. 6;
  subtype Plane_Rotation is Integer range 1 .. 4;

  use HAT;

  procedure Rotate (v : in out Vector; face : Cube_Face; spin : Plane_Rotation) is
    w : Vector;
  begin
    w (3) := v (3);
    case spin is
      when 1 => w (1) :=  v (1); w (2) := v (2);
      --  Rotate around z
      when 2 => w (1) := -v (2); w (2) :=  v (1);  --   90
      when 3 => w (1) := -v (1); w (2) := -v (2);  --  180
      when 4 => w (1) :=  v (2); w (2) := -v (1);  --  -90
    end case;
    --
    case face is
      when 1 => v := w;
      --  Rotate around x axis (z axis turns and joins -y, -z, or +y)
      when 2 => v (1) := w (1); v (2) := -w (3); v (3) :=  w (2);  --   90
      when 3 => v (1) := w (1); v (2) := -w (2); v (3) := -w (3);  --  180
      when 4 => v (1) := w (1); v (2) :=  w (3); v (3) := -w (2);  --  -90
      --  Rotate around y axis (z axis turns and joins -x or x)
      when 5 => v (2) := w (2); v (1) := -w (3); v (3) :=  w (1);  --   90
      when 6 => v (2) := w (2); v (1) :=  w (3); v (3) := -w (1);  --  -90
    end case;
  end Rotate;

  procedure Minus (a, b : Vector; c : out Vector) is
  begin
    for i in 1 .. 3 loop
      c (i) := a (i) - b (i);
    end loop;
  end Minus;

  function Dist_L1 (a, b : Vector) return Natural is
    res : Natural := 0;
  begin
    for i in 1 .. 3 loop
      res := res + abs (a (i) - b (i));
    end loop;
    return res;
  end Dist_L1;

  input : constant VString := +"aoc_2021_19.txt";

  max_scanners : constant := 50;
  max_coords : constant := 50;
  coords : array (1 .. max_scanners) of Natural;
  data : array (1 .. max_scanners, 1 .. max_coords) of Vector;
  scanners : Natural;

  verbose : constant := 1;

  procedure Read_Data is
    v1, v2 : Character;
    f : File_Type;
    s : VString;
  begin
    scanners := 0;
    Open (f, input);
    while not End_Of_File (f) loop
      Get_Line (f, s);  --  Header
      if verbose > 0 then
        Put_Line (s);
      end if;
      scanners := scanners + 1;
      coords (scanners) := 0;
      loop
        coords (scanners) := coords (scanners) + 1;
        Get (f, data (scanners, coords (scanners)) (1));
        Get (f, v1);
        Get (f, data (scanners, coords (scanners)) (2));
        Get (f, v2);
        Get (f, data (scanners, coords (scanners)) (3));
        exit when End_Of_File (f);
        Skip_Line (f);
        exit when End_Of_Line (f);
      end loop;
      exit when End_Of_File (f);
      Skip_Line (f);  --  Empty line
    end loop;
    Close (f);
  end Read_Data;

  type Scanner_Pair_Info is record
    paired : Boolean;
    face   : Cube_Face;
    spin   : Plane_Rotation;
    shift  : Vector;
  end record;

  info : array (1 .. max_scanners, 1 .. max_scanners) of Scanner_Pair_Info;

  procedure Align is
    vs_1, vs_2, vs_1_to_2, vs_2_o, v_test : Vector;
    aligned : Natural;
    found : Boolean;
    threshold : constant := 12;  --  This is given.
  begin
    for s_1 in 1 .. scanners loop
      for s_2 in s_1 + 1 .. scanners loop
        info (s_1, s_2).paired := False;
      end loop;
    end loop;
    --
    Scanner_Loop_1 :
    for s_1 in 1 .. scanners loop
      Aligned_Beacon_1_Search :
      for c1 in 1 .. coords (s_1) - threshold + 1 loop
        vs_1 := data (s_1, c1);
        Scanner_Loop_2 :
        for s_2 in 1 .. scanners loop
          if not (s_1 = s_2 or else info (s_1, s_2).paired) then
            Rotation_Cube :
            for face in Cube_Face loop
              Rotation_Face :
              for spin in Plane_Rotation loop
                Aligned_Beacon_2_Search :
                for cs_2 in 1 .. coords (s_2) loop
                  vs_2 := data (s_2, cs_2);
                  Rotate (vs_2, face, spin);
                  Minus (vs_2, vs_1, vs_1_to_2);
                  --  Now, try to align all data
                  --  between scanner s_1 and scanner s_2
                  aligned := 0;
                  found := False;
                  --
                  --  Try matching any data of scanner 2 with any data of scanner 1.
                  --
                  for cs_1_o in 1 .. coords (s_1) loop
                    for cs_2_o in 1 .. coords (s_2) loop
                      vs_2_o := data (s_2, cs_2_o);
                      Rotate (vs_2_o, face, spin);
                      Minus (vs_2_o, vs_1_to_2, v_test);
                      if Dist_L1 (v_test, data (s_1, cs_1_o)) = 0 then
                        aligned := aligned + 1;
                      end if;
                    end loop;
                  end loop;
                  --
                  found := aligned >= threshold;
                  if found then
                    info (s_1, s_2).paired := True;
                    info (s_1, s_2).face   := face;
                    info (s_1, s_2).spin   := spin;
                    info (s_1, s_2).shift  := vs_1_to_2;
                  end if;
                  if verbose > 0 and found then
                    Put_Line (
                      +"aligned: " & aligned &
                       " beacons between scanners " & (s_1 - 1) & " and " & (s_2 - 1) &
                       " translation vector: " &
                       vs_1_to_2 (1) & ", " & vs_1_to_2 (2) & ", " & vs_1_to_2 (3));
                  end if;
                  exit when found;  --  We could do a multiple loop exit, but HAC 0.098 can't yet.
                end loop Aligned_Beacon_2_Search;
                exit when found;    --  We could do a multiple loop exit, but HAC 0.098 can't yet.
              end loop Rotation_Face;
              exit when found;      --  We could do a multiple loop exit, but HAC 0.098 can't yet.
            end loop Rotation_Cube;
            exit when found;        --  We could do a multiple loop exit, but HAC 0.098 can't yet.
          end if;
        end loop Scanner_Loop_2;
      end loop Aligned_Beacon_1_Search;
    end loop Scanner_Loop_1;
  end Align;

  max_beacons : constant := 2500;  --  "full Ada": max_scanners * max_coords
  beacons : Natural := 0;
  beacon : array (1 .. max_beacons) of Vector;

  scanner_pos : array (1 .. max_scanners) of Vector;

  procedure Make_Beacon_List is
    --  Shortcut in the connectivity tree towards the root (first scanner).
    up : array (2 .. max_scanners) of Natural;
    connected : Natural := 0;
    found : Boolean;
    s : Positive;
    v, vt : Vector;
  begin
    for child_node in 2 .. scanners loop
      up (child_node) := 0;
      if info (1, child_node).paired then
        up (child_node) := 1;
        connected := connected + 1;
      end if;
    end loop;
    while connected < scanners - 1 loop
      for child_node in 2 .. scanners loop
        if up (child_node) = 0 then
          for parent_node in 2 .. scanners loop
            if info (parent_node, child_node).paired
              and then up (parent_node) /= 0  --  Check that the parent is itself connected!
            then
              up (child_node) := parent_node;
              connected := connected + 1;
              exit;
            end if;
          end loop;
        end if;
      end loop;
    end loop;
    --
    beacons := coords (1);
    for i in 1 .. beacons loop
      beacon (i) := data (1, i);
    end loop;
    --
    for d in 1 .. 3 loop
      scanner_pos (1) (d) := 0;
    end loop;
    for i in 2 .. scanners loop
      if verbose > 0 then
        Put_Line (+"Convert data from scanner " & (i - 1) & " to scanner 0");
      end if;
      for j in 1 .. coords (i) loop
        v := data (i, j);
        s := i;
        while s /= 1 loop
          if verbose > 0 and j = 1 then
            Put_Line (+"  Convert data from scanner " & (s - 1) & " to scanner " & (up (s) - 1));
          end if;
          Rotate (v, info (up (s), s).face, info (up (s), s).spin);
          Minus (v, info (up (s), s).shift, vt);
          v := vt;
          s := up (s);
        end loop;
        found := False;
        for k in 1 .. beacons loop
          if Dist_L1 (vt, beacon (k)) = 0 then
            found := True;
            exit;
          end if;
        end loop;
        if not found then
          beacons := beacons + 1;
          beacon (beacons) := v;
        end if;
      end loop;
      for d in 1 .. 3 loop
        v (d) := 0;
      end loop;
      s := i;
      while s /= 1 loop
        Rotate (v, info (up (s), s).face, info (up (s), s).spin);
        Minus (v, info (up (s), s).shift, vt);
        v := vt;
        s := up (s);
      end loop;
      scanner_pos (i) := v;
    end loop;
    if verbose > 1 then
      for k in 1 .. beacons loop
        for d in 1 .. 3 loop
          Put (beacon (k)(d)); Put (' ');
        end loop;
        New_Line;
      end loop;
    end if;
  end Make_Beacon_List;

  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  Align;
  Make_Beacon_List;
  r (1) := beacons;
  r (2) := 0;
  for i in 1 .. scanners loop
    for j in i + 1 .. scanners loop
      r (2) := Max (r (2), Dist_L1 (scanner_pos (i), scanner_pos (j)));
    end loop;
  end loop;
  --
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: Number of beacons            : " & r (1));
    Put_Line (+"Part 2: Max distance between scanners: " & r (2));
    --  Part 1: validated by AoC: 408
    --  Part 2: validated by AoC: 13348
  end if;
end AoC_2021_19;
