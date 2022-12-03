--  Solution to Advent of Code 2022, Day 3
------------------------------------------
--  Rucksack Reorganization
--
--  https://adventofcode.com/2022/day/3
--  Copy of questions in: aoc_2022_03_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_03 is
  use HAT;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

  si : Character;
  f : File_Type;
  s : VString;

  function Score (c : Character) return Natural is
  begin
    case c is
      when 'a' .. 'z' =>
        return 1 + Character'Pos (c) - Character'Pos ('a');
      when 'A' .. 'Z' =>
        return 27 + Character'Pos (c) - Character'Pos ('A');
      when others =>
        return 0;
    end case;
  end Score;

  type Set is array (Character) of Boolean;
  item : Set;
  group : array (0 .. 2) of Set;

  procedure Reset is
  begin
    for i in group'Range loop
      for c in Character loop
        group (i)(c) := False;
      end loop;
    end loop;
  end Reset;

  line, t1, t2 : Natural := 0;
  count : Natural;

begin
  Reset;
  Open (f, "aoc_2022_03.txt");
  while not End_Of_File (f) loop
    Get_Line (f, s);
    --  Part 1's job: add the priority of the item that
    --                is on both halves of each sack.
    for c in Character loop
      item (c) := False;
    end loop;
    for i in 1 .. Length (s) / 2 loop
      item (Element (s, i)) := True;
    end loop;
    for i in Length (s) / 2 + 1 .. Length (s) loop
      si := Element (s, i);
      if item (si) then
        t1 := t1 + Score (si);
        exit;
      end if;
    end loop;
    --  Part 2's job: add priority of the item that is on
    --                each sack of a group of three.
    for i in 1 .. Length (s) loop
      group (line mod 3)(Element (s, i)) := True;
    end loop;
    line := line + 1;
    if line mod 3 = 0 then
      for c in Character loop
        count := 0;
        for i in group'Range loop
          if group (i)(c) then
            count := count + 1;
            if count = 3 then
              t2 := t2 + Score (c);
            end if;
          end if;
        end loop;
      end loop;
      Reset;
    end if;
  end loop;
  Close (f);
  if compiler_test_mode then
    if t1 /= Integer'Value (To_String (Argument (1))) or
       t2 /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Priorities of item present in ...");
    Put_Line (+" (part 1) both half-sacks . . . . . . . : " & Image (t1));
    Put_Line (+" (part 2) all sacks of a group of three : " & Image (t2));
    --  Part 1: validated by AoC: 8185
    --  Part 2: validated by AoC: 2817
  end if;
end AoC_2022_03;
