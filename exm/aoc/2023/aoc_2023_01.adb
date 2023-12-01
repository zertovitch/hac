--  Solution to Advent of Code 2023, Day 1
------------------------------------------
--  Trebuchet?!
--
--  https://adventofcode.com/2023/day/1
--  Copy of questions in: aoc_2023_01_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_01 is
  use AoC_Toolbox, HAT;
  --
  total : array (Part_Type) of Natural;
  --
  procedure Read_Data is
  --  input : constant VString := +"mini.txt";
    input : constant VString := +"aoc_2023_01.txt";
    --
    c : Character;
    f : File_Type;
    s : VString;
    first_digit : array (Part_Type) of Boolean;
    n1p, n2p : array (Part_Type) of Digit_Type;

    procedure Test_Word (pos : Positive; n : Digit_Type) is
      res : Integer := -1;
    begin
      case n is
        when 0 => if Index (s, "zero",  pos) = pos then res := n; end if;
        when 1 => if Index (s, "one",   pos) = pos then res := n; end if;
        when 2 => if Index (s, "two",   pos) = pos then res := n; end if;
        when 3 => if Index (s, "three", pos) = pos then res := n; end if;
        when 4 => if Index (s, "four",  pos) = pos then res := n; end if;
        when 5 => if Index (s, "five",  pos) = pos then res := n; end if;
        when 6 => if Index (s, "six",   pos) = pos then res := n; end if;
        when 7 => if Index (s, "seven", pos) = pos then res := n; end if;
        when 8 => if Index (s, "eight", pos) = pos then res := n; end if;
        when 9 => if Index (s, "nine",  pos) = pos then res := n; end if;
      end case;
      if res >= 0 then
        n2p (part_2) := res;
        if first_digit (part_2) then
          n1p (part_2) := n2p (part_2);
          first_digit (part_2) := False;
        end if;
      end if;
    end Test_Word;

    procedure Process_Digit (n : Digit_Type) is
    begin
      for part in Part_Type loop
        if first_digit (part) then
          n1p (part) := n;
          first_digit (part) := False;
        end if;
        n2p (part) := n;
      end loop;
    end Process_Digit;

  begin
    for part in Part_Type loop
      total (part) := 0;
    end loop;
    Open (f, input);
    while not End_Of_File (f) loop
      for part in Part_Type loop
        first_digit (part) := True;
      end loop;
      Get_Line (f, s);
      for i in 1 .. Length (s) loop
        c := Element (s, i);
        case c is
          when '0' .. '9' =>
            Process_Digit (Ord (c) - Ord ('0'));
          when 'z' =>
            Test_Word (i, 0);
          when 'o' =>
            Test_Word (i, 1);
          when 't' =>
            Test_Word (i, 2);
            Test_Word (i, 3);
          when 'f' =>
            Test_Word (i, 4);
            Test_Word (i, 5);
          when 's' =>
            Test_Word (i, 6);
            Test_Word (i, 7);
          when 'e' =>
            Test_Word (i, 8);
          when 'n' =>
            Test_Word (i, 9);
          when others =>
            null;
        end case;
      end loop;
      for part in Part_Type loop
        total (part) := total (part) + 10 * n1p (part) + n2p (part);
      end loop;
    end loop;
    Close (f);
  end Read_Data;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  if compiler_test_mode then
    if total (part_1) /= Integer_Value (Argument (1)) or
       total (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : " & total (part_1));
    Put_Line (+"Part 2: : " & total (part_2));
    --  Part 1: validated by AoC: 52974
    --  Part 2: validated by AoC: 53340
  end if;
end AoC_2023_01;
