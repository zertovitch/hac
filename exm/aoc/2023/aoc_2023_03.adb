--  Solution to Advent of Code 2023, Day 3
------------------------------------------
--  Gear Ratios
--
--  https://adventofcode.com/2023/day/3
--  Copy of questions in: aoc_2023_03_questions.txt
--

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_03 is
  use AoC_Toolbox, HAT;
  --
  --
  r : array (Part_Type) of Integer;
  --
  procedure Read_Data is
  --  input : constant VString := +"mini.txt"; size : constant := 12;
    input : constant VString := +"aoc_2023_03.txt"; size : constant := 142;
    --
    f : File_Type;
    c : Character;
    --
    subtype Row is String (1 .. size);
    t : array (0 .. 2) of Row;  --  Window of 3 rows.
    --
    type Gear_Info is record
      adjacent_numbers : Natural;
      ratio            : Natural;
    end record;
    type Gear_Row is array (1 .. size) of Gear_Info;
    g : array (0 .. 2) of Gear_Row;
    --
    procedure Clear (gr : in out Gear_Row) is
    begin
      for j in gr'Range loop
        gr (j).adjacent_numbers := 0;
        gr (j).ratio := 1;
      end loop;
    end Clear;
    --
    n : Integer;
    procedure Mark_Gear (i, j : Natural) is
    begin
      if t (i)(j) = '*' then
        g (i)(j).adjacent_numbers := g (i)(j).adjacent_numbers + 1;
        g (i)(j).ratio := g (i)(j).ratio * n;
      end if;
    end Mark_Gear;
    --
    cur : Integer := 1;
    j1, j2 : Integer;
    sym : Boolean;
    --
    procedure Analyse is
      prev_1 : constant Integer := (cur - 1) mod 3;
      prev_2 : constant Integer := (cur - 2) mod 3;
    begin
      j1 := 2;
      loop
        if t (prev_1)(j1) in '0' .. '9' then
          j2 := j1;
          loop
            exit when t (prev_1)(j2 + 1) not in '0' .. '9';
            j2 := j2 + 1;
          end loop;
          --  Now t (cur-1)(j1 .. j2) contains a number
          --  n := Integer'Value (t (cur-1)(j1 .. j2));  Nice to have in HAC...
          n := 0;
          for j in j1 .. j2 loop
            n := n * 10 + Ord (t (prev_1)(j)) - Ord ('0');
          end loop;
          --  Now, we look at symbols around the number.
          sym := False;
          for j in j1 - 1 .. j2 + 1 loop
            c := t (prev_2)(j);  --  2 rows before
            if not (c = '.' or c in '0' .. '9') then
              sym := True;
              Mark_Gear (prev_2, j);
            end if;
            c := t (cur)(j);  --  Current row
            if not (c = '.' or c in '0' .. '9') then
              sym := True;
              Mark_Gear (cur, j);
            end if;
          end loop;
          --  Previous row (being analysed): left and right character.
          sym := sym or (t (prev_1)(j1 - 1) /= '.') or (t (prev_1)(j2 + 1) /= '.');
          Mark_Gear (prev_1, j1 - 1);  --  Left
          Mark_Gear (prev_1, j2 + 1);  --  Right
          if sym then
            --  Add numbers with an adjacent symbol:
            r (part_1) := r (part_1) + n;
          end if;
          j1 := j2 + 1;
        else
          j1 := j1 + 1;
        end if;
        exit when j1 >= size - 1;
      end loop;
      --  Collect the complete gear info
      for j in 2 .. size - 1 loop
        if g (prev_2)(j).adjacent_numbers = 2 then
          --  Sum the gear ratios
          r (part_2) := r (part_2) + g (prev_2)(j).ratio;
        end if;
      end loop;
    end Analyse;
  begin
    --  Clear the window
    for i in 0 .. 2 loop
      for j in 1 .. size loop
        t (i)(j) := '.';
      end loop;
      Clear (g (i));
    end loop;
    Open (f, input);
    while not End_Of_File (f) loop
      for j in 2 .. size - 1 loop
        Get (f, t (cur)(j));
      end loop;
      --  Analyse the *previous* row.
      Analyse;
      cur := (cur + 1) mod 3;
      Clear (g (cur));
    end loop;
    Close (f);
    for j in 1 .. size loop
      t (cur)(j) := '.';
    end loop;
    Analyse;
  end Read_Data;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
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
    --  Part 1: validated by AoC: 539590
    --  Part 2: validated by AoC: 80703636
  end if;
end AoC_2023_03;
