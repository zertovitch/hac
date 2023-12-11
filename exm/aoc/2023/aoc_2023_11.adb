--  Solution to Advent of Code 2023, Day 11
-------------------------------------------
--  Cosmic Expansion
--
--  https://adventofcode.com/2023/day/11
--  Copy of questions in: aoc_2023_11_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_11 is

  use AoC_Toolbox, HAT;

  --
  --  input_name : constant VString := +"mini"; n : constant := 10;
  input_name : constant VString := +"aoc_2023_11"; n : constant := 140;
  
  gala_max : constant := 500;
  gala_top : Natural := 0;
  
  pt_compact, pt : array (1 .. gala_max) of Point;

  empty_row, empty_col : array (1..n) of Boolean;
  
  new_x, new_y : array (1..n) of integer;

  procedure Read_Data is
    c, sep : Character;
    asm : String (1 .. 3);  
    f : File_Type;
    s : VString;
  begin
    for xy in 1 .. n loop
      empty_row (xy):= True;
      empty_col (xy):= True;
    end loop;
    Open (f, input_name & ".txt");
    for y in 1 .. n loop
      for x in 1 .. n loop
        Get (f, c);
        if c = '#' then
          gala_top := gala_top + 1;
          pt_compact(gala_top).x := x;
          pt_compact(gala_top).y := y;
          empty_col(x) := False;
          empty_row(y) := False;
        end if;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer;

  procedure Expand_Universe (new_gap : Positive) is
  begin
    for xy in 1 .. n loop
      new_x (xy):= xy;
      new_y (xy):= xy;
    end loop;
    for xy in reverse 1 .. n loop
      if empty_col(xy) then
        for idx in xy .. n loop
          new_x (idx) := new_x (idx) + new_gap - 1;
        end loop;
      end if;
      if empty_row(xy) then
        for idx in xy .. n loop
          new_y (idx) := new_y (idx) + new_gap - 1;
        end loop;
      end if;
    end loop;
    for i in 1 .. gala_top loop
      pt (i).x := new_x (pt_compact (i).x);
      pt (i).y := new_y (pt_compact (i).y);
    end loop;
  end;

  procedure Do_Part (p : Part_Type) is
  begin
    case p is
      when part_1 => Expand_Universe (2);
      when part_2 => Expand_Universe (1_000_000);
    end case;
    for i in 1 .. gala_top - 1 loop
      for j in i+1 .. gala_top loop
        r(p):=r(p) + dist_L1 (pt(i), pt(j));
      end loop;
    end loop;
  end;

  procedure Do_Part_2 is
  begin
    null;
  end;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := True;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  for p in Part_Type loop
    Do_Part (p);
  end loop;
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : " & r (part_1));
    Put_Line (+"Part 2: : " & r (part_2));
    --  Part 1: validated by AoC: 10173804
    --  Part 2: validated by AoC: 634324905172
  end if;
end AoC_2023_11;
