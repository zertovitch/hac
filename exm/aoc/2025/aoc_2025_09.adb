--  Solution to Advent of Code 2025, Day 9
------------------------------------------
--  .
--
--  https://adventofcode.com/2025/day/9
--  Copy of questions in: aoc_2025_09_questions.txt
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

procedure AoC_2025_09 is

  use AoC_Toolbox, HAT, Interfaces;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2025_09";

  red : array (1 .. 1000) of Point;
  n : Natural := 0;

  r : array (Part_Type) of VString;

  procedure Read_Data is
    unused_separator : Character;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      n:= n + 1;
      Get (f, red(n).x);
      Get (f, unused_separator);
      Get (f, red(n).y);
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
    s, s_max : Integer_64;
  begin
    s_max := 0;
    for i in 1 .. n loop
      for j in i + 1 .. n loop
        s := Integer_64(abs(red(i).x-red(j).x)+1) * Integer_64(abs(red(i).y-red(j).y)+1);
        if s > s_max then
          s_max := s;
        end if;
      end loop;
    end loop;
    r (part_1) := +"" & Trim_Left (+Integer_64'Image(s_max));
  end Do_Part_1;
  
  procedure Do_Part_2 is
    s, s_max : Integer_64;
    mouth_top    : constant := 50174;  --  Top part of the pacman's mouth.
    mouth_bottom : constant := 48596;  --  Bottom part of the pacman's mouth.
    mouth_x      : constant := 94582;
    is_top, ok, point_k_in_rect : Boolean;
  begin
    s_max := 0;
    for i in 1 .. n loop
      if red(i).x = mouth_x and then (red(i).y = mouth_top or else red(i).y = mouth_bottom) then
        is_top := red(i).y = mouth_top;
        for j in 1 .. n loop
          if red(j).x < mouth_x then
            if        (is_top and then red(j).y > mouth_top)           --  Point j above mouth.
              or else ((not is_top) and then red(j).y < mouth_bottom)  --  Point j below mouth.
            then
              ok := True;
              for k in 1 .. n loop
                point_k_in_rect := red(k).x in red(j).x + 1.. mouth_x - 1;
                if is_top then
                  point_k_in_rect := point_k_in_rect and then red(k).y in mouth_top + 1.. red(j).y - 1;
                else
                  point_k_in_rect := point_k_in_rect and then red(k).y in red(j).y + 1 .. mouth_bottom - 1;
                end if;
                ok := ok and not point_k_in_rect;
                exit when not ok;
              end loop;
              if ok then
                s := Integer_64(abs(red(i).x-red(j).x)+1) * Integer_64(abs(red(i).y-red(j).y)+1);
              end if;
              if s > s_max then
                s_max := s;
              end if;
            end if;        
          end if;        
        end loop;
      end if;        
    end loop;
    r (part_2) := +"" & Trim_Left (+Integer_64'Image(s_max));
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;
  Do_Part_2;
  if compiler_test_mode then
    if r (part_1) /= Argument (1) or r (part_2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 4749838800.
    --  Part 2: validated by AoC: 1624057680.
  end if;
end;
