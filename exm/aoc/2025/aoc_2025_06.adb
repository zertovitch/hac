--  Solution to Advent of Code 2025, Day 6
------------------------------------------
--  Trash Compactor
--
--  https://adventofcode.com/2025/day/6
--  Copy of questions in: aoc_2025_06_questions.txt
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

procedure AoC_2025_06 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; l : constant := 15; m : constant := 3; n : constant := 4;
  input_name : constant VString := +"aoc_2025_06"; l : constant := 3772; m : constant := 4; n : constant := 1000;

  a : array (1 .. m, 1 .. n) of Integer;

  op : array (1 .. n) of Character;
  pos_op : array (1 .. n) of Positive;
  dig_it : array (1 .. m, 1 .. l) of Character;

  --  r : array (Part_Type) of Integer;
  r : array (Part_Type) of VString;

  procedure Read_Data is
    k : Integer;
    f : File_Type;
    s : VString;
  begin
    --  Read the numbers:
    Open (f, input_name & ".txt");
    for i in 1 .. m loop
      for j in 1 .. n loop
        Get (f, a (i, j));
      end loop;
    end loop;
    Close (f);

    --  Parse operator row:
    Open (f, input_name & ".txt");
    Skip_Line (f, m);
    Get_Line (f, s);
    Close (f);
    k := 0;
    for i in 1 .. Length (s) loop
      case Element (s, i) is
        when ' ' =>
          null;
        when others =>
          k := k + 1;
          pos_op (k) := i;
          --  put_line(i);
          op (k) := Element (s, i);
      end case;
    end loop;

    --  Parse digits or blanks individually for Part 2:
    Open (f, input_name & ".txt");
    for i in 1 .. m loop
      for j in 1 .. l loop
        Get (f, dig_it (i, j));
      end loop;
    end loop;
    Close (f);

  end Read_Data;

  procedure Do_Part_1 is
    total : Integer := 0;
    sumprod : Integer;
  begin
    for j in 1 .. n loop
      if op (j) = '+' then
        sumprod := 0;
      else
        sumprod := 1;
      end if;
      for i in 1 .. m loop
        if op (j) = '+' then
          sumprod := sumprod + a (i, j);
        else
          sumprod := sumprod * a (i, j);
        end if;
      end loop;
      total := total + sumprod;
    end loop;

    r (part_1) := +"" & total;
  end Do_Part_1;

  procedure Do_Part_2 is
    last : Integer;
    s : VString;
    total : Integer := 0;
    sumprod : Integer;
  begin
    for j in 1 .. n loop
      if j = n then
        last := l;
      else
        last := pos_op (j + 1) - 2;
      end if;
      if op (j) = '+' then
        sumprod := 0;
      else
        sumprod := 1;
      end if;
      --  Parse each cephalopod number, with possible
      --  leading or trailing blanks:
      for k in pos_op (j) .. last loop
        s := +"";
        for i in 1 .. m loop
          s := s & dig_it (i, k);
        end loop;
        if op (j) = '+' then
          sumprod := sumprod + Integer_Value (s);
        else
          sumprod := sumprod * Integer_Value (s);
        end if;
      end loop;
      --  NB: the "right-to-left" mention in "Cephalopod math is
      --  written right-to-left in columns" is irrelevant, it's
      --  just to confuse readers.
      total := total + sumprod;
    end loop;
    r (part_2) := +"" & total;
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
    --  Part 1: validated by AoC: 4449991244405.
    --  Part 2: validated by AoC: 9348430857627.
  end if;
end AoC_2025_06;
