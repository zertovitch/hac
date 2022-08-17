--  Solution to Advent of Code 2021, Day 2
------------------------------------------
--  Dive!
--
--  https://adventofcode.com/2021/day/2
--  Copy of questions in: aoc_2021_02_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_02 is
  use HAT;
  c, sep : Character;
  i, x, d, aim : Integer;
  f : File_Type;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
begin
  for part in 1 .. 2 loop
    x := 0;
    d := 0;
    aim := 0;
    Open (f, "aoc_2021_02.txt");
    while not End_Of_File (f) loop
      Get (f, c);
      loop
        Get (f, sep);
        exit when sep = ' ';
      end loop;
      Get (f, i);
      case part is
        when 1 =>
          case c is
            when 'f' => x := x + i;  --  Forward
            when 'u' => d := d - i;  --  Up
            when 'd' => d := d + i;  --  Down
            when others => null;
          end case;
        when 2 =>
          case c is
            when 'f' => x := x + i;        --  Forward
                        d := d + i * aim;
            when 'u' => aim := aim - i;    --  Up
            when 'd' => aim := aim + i;    --  Down
            when others => null;
          end case;
      end case;
    end loop;
    Close (f);
    if compiler_test_mode then
      if x * d /= Integer_Value (Argument (part)) then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
    else
      Put_Line (+"Part " & part & ": x = " & x & ", d = " & d & ", solution = x * d = " & x * d);
      --  Part 1: validated by AoC: 2187380
      --  Part 2: validated by AoC: 2086357770
    end if;
  end loop;
end AoC_2021_02;
