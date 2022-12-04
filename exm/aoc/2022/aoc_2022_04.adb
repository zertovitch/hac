--  Solution to Advent of Code 2022, Day 4
------------------------------------------
--  Camp Cleanup
--
--  https://adventofcode.com/2022/day/4
--  Copy of questions in: aoc_2022_04_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_04 is
  use HAT;

  T0 : constant Time := Clock;
  f : File_Type;
  sep1, sep2, sep3 : Character;
  l1, u1, l2, u2 : Natural;
  t1, t2 : Natural := 0;

begin
  Open (f, "aoc_2022_04.txt");
  while not End_Of_File (f) loop
    --  Read data in the form: "2-4,6-8":
    Get (f, l1);
    Get (f, sep1);
    Get (f, u1);
    Get (f, sep2);
    Get (f, l2);
    Get (f, sep3);
    Get (f, u2);
    if (l1 >= l2 and u1 <= u2) or (l2 >= l1 and u2 <= u1) then
      --  Count redundancies:
      t1 := t1 + 1;
    end if;
    if l1 in l2 .. u2 or l2 in l1 .. u1 or
       u1 in l2 .. u2 or u2 in l1 .. u1
    then
      --  Count overlaps:
      t2 := t2 + 1;
    end if;
  end loop;
  Close (f);

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if t1 /= Integer'Value (To_String (Argument (1))) or
       t2 /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: pairs with redundancies . . : " & Image (t1));
    Put_Line (+"Part 2: pairs with overlaps . . . . : " & Image (t2));
    --  Part 1: validated by AoC: 657
    --  Part 2: validated by AoC: 938
  end if;
end AoC_2022_04;
