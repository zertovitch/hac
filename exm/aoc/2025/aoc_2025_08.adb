--  Solution to Advent of Code 2025, Day 8
------------------------------------------
--  Playground.
--
--  https://adventofcode.com/2025/day/8
--  Copy of questions in: aoc_2025_08_questions.txt
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

procedure AoC_2025_08 is

  use AoC_Toolbox, HAT, Interfaces;

  n : Natural := 0;
  rounds : Positive;
  input_name : VString;

  r : array (Part_Type) of VString;

  subtype max_range is Integer range 1 .. 1000;

  box : array (max_range) of Point_3D;

  procedure Read_Data is
    dummy_comma : Character;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      n := n + 1;
      Get (f, box (n).x);
      Get (f, dummy_comma);
      Get (f, box (n).y);
      Get (f, dummy_comma);
      Get (f, box (n).z);
    end loop;
    Close (f);
  end Read_Data;

  verbose : constant Boolean := False;

  procedure Do_Parts is
    size, size_1, size_2, size_3, max_size : Natural;
    best_i, best_j : Positive;
    best, dist2 : Integer_64;
    pair_connected : array (max_range, max_range) of Boolean;  --  We use only the upper triangle (i < j).
    circuit : array (max_range) of Natural;
    c_j : Positive;
  begin
    for i in 1 .. n loop
      circuit (i) := i;
      for j in 1 .. n loop
        pair_connected (i, j) := False;
      end loop;
    end loop;

    for round in Positive loop
      best := Integer_64'Last;
      for i in 1 .. n loop
        for j in i + 1 .. n loop

          dist2 :=
            Integer_64 (box (i).x - box (j).x) ** 2 +
            Integer_64 (box (i).y - box (j).y) ** 2 +
            Integer_64 (box (i).z - box (j).z) ** 2;

          if dist2 < best and then not pair_connected (i, j) then
            best := dist2;
            best_i := i;
            best_j := j;
          end if;

        end loop;
      end loop;

      pair_connected (best_i, best_j) := True;

      --  Merge circuits.
      for i in 1 .. n loop
        for j in i + 1 .. n loop
          if pair_connected (i, j) then
            c_j := circuit (j);
            for k in 1 .. n loop
              if circuit (k) = c_j then
                circuit (k) := circuit (i);
              end if;
            end loop;
          end if;
        end loop;
      end loop;

      if round = rounds then
        size_1 := 1;
        size_2 := 1;
        size_3 := 1;
        for c in 1 .. n loop
          size := 0;
          for i in 1 .. n loop
            if circuit (i) = c then
              size := size + 1;
            end if;
          end loop;

          if verbose then
            if size > 1 then
              Put_Line (+"Size: " & size);
              for i in 1 .. n loop
                if circuit (i) = c then
                  Put_Line (+"    " & box (i).x & ", " & box (i).y & ", " & box (i).z);
                end if;
              end loop;
            end if;
          end if;

          if size > size_1 then
            size_3 := size_2;
            size_2 := size_1;
            size_1 := size;
          elsif size > size_2 then
            size_3 := size_2;
            size_2 := size;
          elsif size > size_3 then
            size_3 := size;
          end if;
        end loop;
        r (part_1) := +"" & size_1 * size_2 * size_3;
      end if;

      if verbose then
        Put_Line (+"Circuit list, round " & round);
        for c in 1 .. n loop
          size := 0;
          for i in 1 .. n loop
            if circuit (i) = c then
              size := size + 1;
            end if;
          end loop;
          Put_Line (+"  circuit " & c & ": " & size);
        end loop;
      end if;

      max_size := 0;
      for c in 1 .. n loop
        size := 0;
        for i in 1 .. n loop
          if circuit (i) = c then
            size := size + 1;
          end if;
        end loop;
        max_size := Max (max_size, size);
      end loop;

      if max_size = n then
        if verbose then
          Put (+"" & box (best_i).x & ", " & box (best_j).x);
          Put_Line (+"Final round = " & round);
        end if;
        r (part_2) :=
          Trim_Both (+"" & Integer_64'Image (Integer_64 (box (best_i).x) * Integer_64 (box (best_j).x)));
        exit;
      end if;

    end loop;
  end Do_Parts;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin

  if compiler_test_mode then
    input_name := +"aoc_2025_08_mini";
    rounds := 10;
  else
    input_name := +"aoc_2025_08";
    rounds := 1000;
  end if;

  Read_Data;
  Do_Parts;

  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 72150 (mini: 40).
    --  Part 2: validated by AoC: 3926518899 (mini: 25272).
  end if;
end AoC_2025_08;
