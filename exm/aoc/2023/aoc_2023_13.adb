--  Solution to Advent of Code 2023, Day 13
-------------------------------------------
--  Point of Incidence
--
--  https://adventofcode.com/2023/day/13
--  Copy of questions in: aoc_2023_13_questions.txt
--
--  HAC 0.26 "nice to have"'s detected in this exercise:
--    *     Unsigned_X and logical operators on it.

--  The files aoc_toolbox.ad* are located in the upper directory (..)
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_13 is

  use AoC_Toolbox, HAT;

  maxi : constant := 31;

  subtype Max_Range is Natural range 0 .. maxi;

  map : array (Max_Range, Max_Range) of Boolean;
  row, col : array (Max_Range) of Integer;
  m, n : Integer;

  procedure Prepare_Binary_Numbers is
    a : Natural;
    x : Positive;
  begin
    for i in 1 .. m loop
      a := 0;
      x := 1;
      for j in reverse 1 .. n loop
        if map (i, j) then
          a := a + x;
        end if;
        x := x * 2;
      end loop;
      row (i) := a;
    end loop;
    for j in 1 .. n loop
      a := 0;
      x := 1;
      for i in reverse 1 .. m loop
        if map (i, j) then
          a := a + x;
        end if;
        x := x * 2;
      end loop;
      col (j) := a;
    end loop;
  end Prepare_Binary_Numbers;

  function Calc_Ref_Code (old_rc_to_avoid : Natural) return Natural is
    sym : Boolean;
    ref_code : Integer;
    s1, s2 : Integer;
  begin
    --  Find vertical reflection:
    for j in 1 .. n - 1 loop
      sym := True;
      for jj in 0 .. n loop
        s1 := j - jj;
        s2 := j + jj + 1;
        exit when s1 not in 1 .. n or else s2 not in 1 .. n;
        sym := sym and then col (s1) = col (s2);
        exit when not sym;
      end loop;
      if sym and then j /= old_rc_to_avoid then
        return j;
      end if;
    end loop;
    --  Find horizontal reflection:
    for i in 1 .. m - 1 loop
      sym := True;
      for ii in 0 .. m loop
        s1 := i - ii;
        s2 := i + ii + 1;
        exit when s1 not in 1 .. m or else s2 not in 1 .. m;
        sym := sym and then row (s1) = row (s2);
        exit when not sym;
      end loop;
      if sym then
        ref_code := 100 * i;
        if ref_code /= old_rc_to_avoid then
          return ref_code;
        end if;
      end if;
    end loop;
    --  Hummm... No reflection found ?
    return 0;
  end Calc_Ref_Code;

  r : array (Part_Type) of Integer;

  bit_flipping_method : constant Boolean := True;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2023_13";
  --
  procedure Read_Data_and_Solve is
    f : File_Type;
    s : VString;
    rc, rcs : Natural;
    mem_row, mem_col : Integer;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      m := 0;
      n := 0;
      loop
        Get_Line (f, s);
        exit when Length (s) = 0;
        m := m + 1;
        n := Length (s);
        for j in 1 .. n loop
          map (m, j) := Element (s, j) = '#';
        end loop;
        exit when End_Of_File (f);
      end loop;
      --
      --  Map is loaded.
      --
      Prepare_Binary_Numbers;
      rc := Calc_Ref_Code (0);
      r (part_1) := r (part_1) + rc;
      --
      --  Part 2: after the correction on the mirror
      --  there are one *or two* reflection lines!
      --  So, if we go for correcting the smudge and
      --  check the new reflection line, we have to be
      --  sure that we don't report the old line!
      --
      --  Variant: we could cumulate 1-bit differences
      --  in a variant of the function Calc_Ref_Code and
      --  spot *the* new reflection with an aberration of
      --  exactly 1. This would save the double loop below.
      --
      Smudge :
      for i in 1 .. m loop
        for j in 1 .. n loop
          --  Test flipping the bit on cell (i, j) of the map.
          if bit_flipping_method then
            mem_row := row (i);
            mem_col := col (j);
            --  All this would be easier with Unsigned_X and an "xor" mask...
            if map (i, j) then
              row (i) := row (i) - 2 ** (j - 1);
              col (j) := col (j) - 2 ** (i - 1);
            else
              row (i) := row (i) + 2 ** (j - 1);
              col (j) := col (j) + 2 ** (i - 1);
            end if;
            rcs := Calc_Ref_Code (rc);
            exit Smudge when rcs > 0;
            row (i) := mem_row;
            col (j) := mem_col;
          else
            map (i, j) := not map (i, j);
            Prepare_Binary_Numbers;
            rcs := Calc_Ref_Code (rc);
            exit Smudge when rcs > 0;
            map (i, j) := not map (i, j);
          end if;
        end loop;
      end loop Smudge;
      if rcs = 0 then Put ("New axis not found !!!"); end if;
      r (part_2) := r (part_2) + rcs;
    end loop;
    Close (f);
  end Read_Data_and_Solve;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data_and_Solve;
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 33728
    --  Part 2: validated by AoC: 28235
  end if;
end AoC_2023_13;
