--  Solution to Advent of Code 2024, Day 7
-------------------------------------------
--  Bridge Repair
--
--  https://adventofcode.com/2024/day/7
--  Copy of questions in: aoc_2024_07_questions.txt
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

with Interfaces;

procedure AoC_2024_07 is

  use AoC_Toolbox, HAT, Interfaces;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2024_07";

  n : constant := 100;

  r : array (Part_Type) of Integer_64;

  type Row_Type is array (1 .. n) of Integer_64;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;

  procedure Read_Data is
    left : Integer_64;
    len_left : Integer;
    data : Row_Type;
    last : Natural := 0;

    function Check_1 return Boolean is
      b : Integer;
      x : Integer_64;
    begin
      if last = 1 and then left = data (1) then
        return True;
      end if;

      for i in 0 .. 2 ** (last - 1) - 1 loop
        b := i;
        x := data (1);
        for pos in 2 .. last loop
          if b mod 2 = 0 then
            x := x + data (pos);
          else
            x := x * data (pos);
          end if;
          exit when x > left;  --  Too big, give up.
          b := b / 2;
        end loop;
        if x = left then
          return True;
        end if;
      end loop;
      return False;
    end Check_1;

    function Check_2 return Boolean is
      b : Integer;
      x : Integer_64;
      concat : VString;
    begin
      if last = 1 and then left = data (1) then
        return True;
      end if;

      for i in 0 .. 3 ** (last - 1) - 1 loop
        b := i;
        x := data (1);
        for pos in 2 .. last loop
          case b mod 3 is
            when 0 =>
              x := x + data (pos);
            when 1 =>
              x := x * data (pos);
            when others =>  --  0
              concat := Image (x) & Image (data (pos));
              if Length (concat) > len_left then
                --  Too big, give up.
                x := 0;
                exit;
              end if;
              x :=  Integer_64'Value (To_String (concat));
          end case;
          exit when x > left;  --  Too big, give up.
          b := b / 3;
        end loop;
        if x = left then
          return True;
        end if;
      end loop;
      return False;
    end Check_2;

    sep : Character;
    f : File_Type;

  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      if last = 0 then
        Get (f, left);
        Get (f, sep);
        len_left := Length (Image (left));
      end if;
      last := last + 1;
      Get (f, data (last));

      if End_Of_Line (f) or End_Of_File (f) then
        if Check_1 then
          r (part_1) := r (part_1) + left;
        end if;
        if (not compiler_test_mode) and then Check_2 then
          r (part_2) := r (part_2) + left;
        end if;

        last := 0;  --  Clear data (calculator terms & factors).
      end if;

    end loop;
    Close (f);
  end Read_Data;

  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (-Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put (+"Part 1:"); Put (r (part_1)'Image); New_Line;
    Put (+"Part 2:"); Put (r (part_2)'Image); New_Line;
    --  Part 1: validated by AoC: 850435817339
    --  Part 2: validated by AoC: 104824810233437
  end if;
end AoC_2024_07;
