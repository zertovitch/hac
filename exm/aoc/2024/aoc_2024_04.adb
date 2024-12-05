--  Solution to Advent of Code 2024, Day 4
------------------------------------------
--  Ceres Search
--
--  https://adventofcode.com/2024/day/4
--  Copy of questions in: aoc_2024_04_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_04 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 10; n_p_3 : constant := 13;
  input_name : constant VString := +"aoc_2024_04"; n : constant := 140; n_p_3 : constant := 143;

  a : array (-2 .. n_p_3, -2 .. n_p_3) of Character;

  r : array (Part_Type) of Integer;

  procedure Read_Data is
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    for i in a'Range (1) loop
      for j in a'Range (2) loop
        if i in 1 .. n and then j in 1 .. n then
          Get (f, a (i, j));
        else
          a (i, j) := ' ';
        end if;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop

        case a (i, j) is

          when 'X' =>

            --  Search_Ver:
            for fake in 1 .. 1 loop
              exit when a (i + 1, j) /= 'M';
              exit when a (i + 2, j) /= 'A';
              exit when a (i + 3, j) /= 'S';
              r (part_1) := r (part_1) + 1;
            end loop;
            --  Search_Diag:
            for fake in 1 .. 1 loop
              exit when a (i + 1, j + 1) /= 'M';
              exit when a (i + 2, j + 2) /= 'A';
              exit when a (i + 3, j + 3) /= 'S';
              r (part_1) := r (part_1) + 1;
            end loop;
            --  Search_Hor:
            for fake in 1 .. 1 loop
              exit when a (i, j + 1) /= 'M';
              exit when a (i, j + 2) /= 'A';
              exit when a (i, j + 3) /= 'S';
              r (part_1) := r (part_1) + 1;
            end loop;
            --  Search_Counter_Diag:
            for fake in 1 .. 1 loop
              exit when a (i - 1, j + 1) /= 'M';
              exit when a (i - 2, j + 2) /= 'A';
              exit when a (i - 3, j + 3) /= 'S';
              r (part_1) := r (part_1) + 1;
            end loop;

          when 'S' =>

            --  Search_Ver_Inv:
            for fake in 1 .. 1 loop
              exit when a (i + 1, j) /= 'A';
              exit when a (i + 2, j) /= 'M';
              exit when a (i + 3, j) /= 'X';
              r (part_1) := r (part_1) + 1;
            end loop;
            --  Search_Diag_Inv:
            for fake in 1 .. 1 loop
              exit when a (i + 1, j + 1) /= 'A';
              exit when a (i + 2, j + 2) /= 'M';
              exit when a (i + 3, j + 3) /= 'X';
              r (part_1) := r (part_1) + 1;
            end loop;
            --  Search_Hor_Inv:
            for fake in 1 .. 1 loop
              exit when a (i, j + 1) /= 'A';
              exit when a (i, j + 2) /= 'M';
              exit when a (i, j + 3) /= 'X';
              r (part_1) := r (part_1) + 1;
            end loop;
            --  Search_Counter_Diag_Inv:
            for fake in 1 .. 1 loop
              exit when a (i - 1, j + 1) /= 'A';
              exit when a (i - 2, j + 2) /= 'M';
              exit when a (i - 3, j + 3) /= 'X';
              r (part_1) := r (part_1) + 1;
            end loop;

          when others => null;

        end case;

      end loop;
    end loop;
  end Do_Part_1;

  procedure Do_Part_2 is
  begin
    for i in 1 .. n - 2 loop
      for j in 1 .. n - 2 loop

        case a (i, j) is

          when 'M' =>

            --  X1:
            for fake in 1 .. 1 loop
              exit when a (i + 1, j + 1) /= 'A';
              exit when a (i + 2, j + 2) /= 'S';
              exit when a (i,     j + 2) /= 'M';
              exit when a (i + 2, j)     /= 'S';
              r (part_2) := r (part_2) + 1;
            end loop;

            --  X2:
            for fake in 1 .. 1 loop
              exit when a (i + 1, j + 1) /= 'A';
              exit when a (i + 2, j + 2) /= 'S';
              exit when a (i,     j + 2) /= 'S';
              exit when a (i + 2, j)     /= 'M';
              r (part_2) := r (part_2) + 1;
            end loop;

        when 'S' =>

          --  X3:
          for fake in 1 .. 1 loop
            exit when a (i + 1, j + 1) /= 'A';
            exit when a (i + 2, j + 2) /= 'M';
            exit when a (i,     j + 2) /= 'S';
            exit when a (i + 2, j)     /= 'M';
            r (part_2) := r (part_2) + 1;
          end loop;

          --  X4:
          for fake in 1 .. 1 loop
            exit when a (i + 1, j + 1) /= 'A';
            exit when a (i + 2, j + 2) /= 'M';
            exit when a (i,     j + 2) /= 'M';
            exit when a (i + 2, j)     /= 'S';
            r (part_2) := r (part_2) + 1;
          end loop;

          when others => null;

        end case;

      end loop;
    end loop;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 2483
    --  Part 2: validated by AoC: 1925
  end if;
end AoC_2024_04;
