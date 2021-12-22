--  Solution to Advent of Code 2021, Day 22
-------------------------------------------
--  Reactor Reboot
--
--  https://adventofcode.com/2021/day/22
--  Copy of questions in: aoc_2021_22_questions.txt
--
--  GNAT: - runs in 2081 seconds (fast mode)
--        - needs to use Integer_64 for counters.
--
--  HAC 0.098 "nice to have"'s detected in this exercise:
--
--    *     package Interfaces with at least Integer_64 for
--            compatibility with GNAT (Integer is always 32 bits)
--
with HAL;
--  For a build with "full Ada": files hal*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_22 is
  use HAL;

  dim_max : constant := 3;
  subtype Dimension_Range is Integer range 1 .. dim_max;
  type Point is array (Dimension_Range) of Integer;

  type Rule_Type is record
    on        : Boolean;
    low, high : Point;
  end record;

  rules_max : constant := 1000;
  rules : Natural := 0;

  rule : array (1 .. rules_max) of Rule_Type;

  --  Lists of numbers
  --  NB: of course in "full Ada" we would so it in a much smarter way.
  list_length_max : constant := rules_max;
  type List_Array_Type is array (1 .. list_length_max) of Integer;
  type List_Type is record
    top : Natural;
    val : List_Array_Type;
  end record;

  --  Insert and sort ascending
  procedure Insert (list : in out List_Type; num : Integer) is
    ins : Natural := list.top + 1;
  begin
    for i in 1 .. list.top loop
      if num = list.val (i) then
        return;
      elsif num < list.val (i) then
        ins := i;  --  Insert here.
        exit;
      end if;
    end loop;
    for i in reverse ins .. list.top loop
      list.val (i + 1) := list.val (i);
    end loop;
    list.val (ins) := num;
    list.top := list.top + 1;
  end Insert;

  --  We record every point in every dimension.
  coord : array (Dimension_Range) of List_Type;

  procedure Read_Data is
    --  input : constant VString := +"mini.txt";
    input : constant VString := +"aoc_2021_22.txt";
    --
    c, sep : Character;
    num : Integer;
    onoff : String (1 .. 3);
    xyz, dotdot : String (1 .. 2);
    f : File_Type;
  begin
    for d in Dimension_Range loop
      coord (d).top := 0;
    end loop;

    Open (f, input);
    while not End_Of_File (f) loop
      rules := rules + 1;
      Get (f, onoff);
      rule (rules).on := onoff = "on ";
      if not rule (rules).on then
        Get (f, c);
      end if;
      for d in Dimension_Range loop
        Get (f, xyz);
        Get (f, num);
        rule (rules).low (d) := num;
        Insert (coord (d), num);
        Get (f, dotdot);
        Get (f, num);
        rule (rules).high (d) := num;
        Insert (coord (d), num);
        exit when d = dim_max;
        Get (f, sep);
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  r : array (1 .. 2) of Integer;

  procedure Part_1 is
    on : Boolean;
    count : Integer := 0;
  begin
    for x in -50 .. 50 loop
      for y in -50 .. 50 loop
        for z in -50 .. 50 loop
          on := False;
          for r in 1 .. rules loop
            if x in rule (r).low (1) .. rule (r).high (1) and then
               y in rule (r).low (2) .. rule (r).high (2) and then
               z in rule (r).low (3) .. rule (r).high (3)
            then
              on := rule (r).on;
            end if;
          end loop;
          if on then count := count + 1; end if;
        end loop;
      end loop;
    end loop;
    r (1) := count;
  end Part_1;

  procedure Part_2 is
    count : Integer := 0;
    --
    --  When d <= dim_max we test different cuboid edges.
    --  When d = dim_max + 1 we scan the cuboid delimited by
    --  input_edge_1 (included), input_edge_2 (excluded)
    --  through the list of rules, and count it when it is `on`.
    --
    procedure Scan (input_edge_1, input_edge_2 : Point; d : Positive) is
      edge_1, edge_2 : Point;
      inside, on : Boolean;
      volume : Integer;
    begin
      if d = dim_max + 1 then
        --
        on := False;
        for r in 1 .. rules loop
          inside := True;
          for dd in Dimension_Range loop
            inside := inside and then
              input_edge_1 (dd) in rule (r).low (dd) .. rule (r).high (dd);
          end loop;
          if inside then
            on := rule (r).on;
          end if;
        end loop;
        if on then
          volume := 1;
          for dd in Dimension_Range loop
            volume := volume * (input_edge_2 (dd) - input_edge_1 (dd));
          end loop;
          count := count + volume;
        end if;
      else
        for i in 1 .. coord (d).top loop
          edge_1 := input_edge_1;
          edge_2 := input_edge_2;
          edge_1 (d) := coord (d).val (i);
          edge_2 (d) := coord (d).val (i) + 1;
          Scan (edge_1, edge_2, d + 1);
          if i < coord (d).top then
            edge_1 (d) := coord (d).val (i) + 1;
            edge_2 (d) := coord (d).val (i + 1);
            Scan (edge_1, edge_2, d + 1);
          end if;
        end loop;
      end if;
    end Scan;
    --
    bogus_edge_1, bogus_edge_2 : Point;
  begin
    Scan (bogus_edge_1, bogus_edge_2, 1);
    r (2) := count;
  end Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  Part_1;
  Part_2;
  if compiler_test_mode then
   if r (1) /= Integer_Value (Argument (1)) or
      r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : number of cubes which are ""on"" within small region" & r (1));
    Put_Line (+"Part 2: : number of cubes which are ""on""" & r (2));
    --  Part 1: validated by AoC: 587097
    --  Part 2: validated by AoC: 1359673068597669
  end if;
end AoC_2021_22;
