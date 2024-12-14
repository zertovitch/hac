--  Solution to Advent of Code 2024, Day 14
-------------------------------------------
--  Restroom Redoubt
--
--  https://adventofcode.com/2024/day/14
--  Copy of questions in: aoc_2024_14_questions.txt
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

procedure AoC_2024_14 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; nx : constant := 11; ny : constant := 7; nr : constant := 12;
  input_name : constant VString := +"aoc_2024_14"; nx : constant := 101; ny : constant := 103; nr : constant := 500;

  type Robot is record
    p, v : Point;
  end record;

  type Robot_Array is array (1 .. nr) of Robot;

  r0 : Robot_Array;

  r : array (Part_Type) of Integer;

  procedure Read_Data is
    sep1, sep2 : Character;
    peq : String (1 .. 2);
    veq : String (1 .. 3);
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for i in 1 .. nr loop
      Get (f, peq);
      Get (f, r0 (i).p.x);
      Get (f, sep1);
      Get (f, r0 (i).p.y);
      Get (f, veq);
      Get (f, r0 (i).v.x);
      Get (f, sep2);
      Get (f, r0 (i).v.y);
    end loop;
    Close (f);
  end Read_Data;

  procedure Show (ro : Robot_Array) is
    has_robot : Boolean;
  begin
    for y in 0 .. ny - 1 loop
      for x in 0 .. nx - 1 loop
        has_robot := False;
        for i in 1 .. nr loop
          if ro (i).p.x = x and then ro (i).p.y = y then
            has_robot := True;
          end if;
        end loop;
        if has_robot then
          Put ("*");
        else
          Put (" ");
        end if;
      end loop;
      New_Line;
    end loop;
  end Show;

  procedure Evolve (ro : in out Robot_Array) is
  begin
    for i in 1 .. nr loop
      ro (i).p.x := (ro (i).p.x + ro (i).v.x) mod nx;
      ro (i).p.y := (ro (i).p.y + ro (i).v.y) mod ny;
    end loop;
  end Evolve;

  function Safety_Factor (ro : in Robot_Array) return Natural is
    q1, q2, q3, q4 : Natural := 0;
  begin
    for i in 1 .. nr loop
      if    ro (i).p.x in 0 .. nx / 2 - 1 and then ro (i).p.y in 0 .. ny / 2 - 1 then
        q1 := q1 + 1;
      elsif ro (i).p.x in nx / 2 + 1 .. nx and then ro (i).p.y in 0 .. ny / 2 - 1 then
        q2 := q2 + 1;
      elsif ro (i).p.x in 0 .. nx / 2 - 1 and then ro (i).p.y in ny / 2 + 1 .. ny then
        q3 := q3 + 1;
      elsif ro (i).p.x in nx / 2 + 1 .. nx and then ro (i).p.y in ny / 2 + 1 .. ny then
        q4 := q4 + 1;
      end if;
    end loop;
    return q1 * q2 * q3 * q4;
  end Safety_Factor;

  procedure Do_Part_1 is
    ro : Robot_Array := r0;
  begin
    for t in 1 .. 100 loop
      Evolve (ro);
    end loop;
    r (part_1) := Safety_Factor (ro);
  end Do_Part_1;

  --  The threshold is determined heuristically.
  --  A perfectly uniform setup has 125 robots in
  --  each quadrant (safety factor 125**4 = 244,140,625).
  --  If all robots are in a single quadrant, the factor is 0.
  --  If 497 robots are in a quadrant and the other quadrants
  --  contain a robot each, the factor is 497.
  --
  procedure Do_Part_2 is
    ro : Robot_Array := r0;
  begin
    for t in 1 .. nx * ny + 1 loop
      Evolve (ro);
      if Safety_Factor (ro) < 40_000_000 then
        r (part_2) := t;
        Show (ro);
        exit;
      end if;
    end loop;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;
  if not compiler_test_mode then
    Do_Part_2;
  end if;

  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 226236192
    --  Part 2: validated by AoC: 8168
  end if;
end AoC_2024_14;
