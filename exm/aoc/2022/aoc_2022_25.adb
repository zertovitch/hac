--  Solution to Advent of Code 2022, Day 25
-------------------------------------------
--  Full of Hot Air
--
--  https://adventofcode.com/2022/day/25
--  Copy of questions in: aoc_2022_25_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2022.gpr .
with HAT;

--  Interfaces is needed for compiling on both
--  HAC and GNAT (64-bit integer: Integer_64):
with Interfaces;

procedure AoC_2022_25 is
  use HAT, Interfaces;

  f : File_Type;
  s : VString;
  i, k, sum : Integer_64;

begin
  sum := 0;
  Open (f, "aoc_2022_25.txt");
Read_Data :
  while not End_Of_File (f) loop
    Get_Line (f, s);
    i := 0;
    for j in 1 .. Length (s) loop
      case Element (s, j) is
        when '0' => k := 0;
        when '1' => k := 1;
        when '2' => k := 2;
        when '-' => k := -1;
        when '=' => k := -2;
        when others => Put ("Whut?");
      end case;
      i := i * 5 + k;
    end loop;
    sum := sum + i;
  end loop Read_Data;
  Close (f);

  s := +"";
  while sum /= 0 loop
    case sum mod 5 is
      when 0 => s := '0' & s;
      when 1 => s := '1' & s;
      when 2 => s := '2' & s;
      when 3 => s := '=' & s;  sum := sum + 5;  --  -2
      when 4 => s := '-' & s;  sum := sum + 5;  --  -1
      when others => null;
    end case;
    sum := sum / 5;
  end loop;
  Put_Line (s);

  --  Part 1: validated by AoC: 2=-0=01----22-0-1-10
end AoC_2022_25;
