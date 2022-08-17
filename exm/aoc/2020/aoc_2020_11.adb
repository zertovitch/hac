--  Solution to Advent of Code 2020, Day 11
-------------------------------------------
--  Seating System
--
--  https://adventofcode.com/2020/day/11
--
-------------------------------------------------------------------------
--
--  HAC 0.084 version.
--
--  NB: when run from HAC with the full-size data ("aoc_2020_11.txt"),
--      the HAC virtual machine shows its capacity of slowing down your computer ;-) .
--      For performance, better use your preferred full Ada compiler...
--
--  HAC 0.084 "nice to have"'s detected in this exercise:
--
--    *     ` aaa : constant Character := 'a';`
--                       HAC should detect an expression as a static (compile-time-known) value
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_11 is
  w : constant := 10;  h : constant := 10;  name : constant VString := +"aoc_2020_11_mini.txt";
  --  w : constant := 93;  h : constant := 98;  name : constant VString := +"aoc_2020_11.txt";
  --
  type Map_Type is array (1 .. h, 1 .. w) of Character;
  --
  --  empty    : constant Character := 'L';  --  Not used. See remarks above.
  --  occupied : constant Character := '#';  --  Not used. See remarks above.
  --
  procedure Move (
    current_map : in     Map_Type;
    new_map     :    out Map_Type;
    simple_rule : in     Boolean;
    change      :    out Boolean
  )
  is
    function Count_Visible_Occupied (i, j : Positive) return Natural is
      occ : Natural := 0;
      --
      procedure Scan_Direction (di, dj : Integer) is
        ii : Integer := i + di;
        jj : Integer := j + dj;
      begin
        loop
          exit when ii not in 1 .. h;
          exit when jj not in 1 .. w;
          case current_map (ii, jj) is
            when '#'    => occ := occ + 1; exit;
            when 'L'    => exit;
            when others => exit when simple_rule;
          end case;
          ii := ii + di;
          jj := jj + dj;
        end loop;
      end Scan_Direction;
    begin
      for di in -1 .. 1 loop
        for dj in -1 .. 1 loop
          if di /= 0 or dj /= 0 then
            Scan_Direction (di, dj);
          end if;
        end loop;
      end loop;
      return occ;
    end Count_Visible_Occupied;
    --
    threshold : Positive;
  begin
    if simple_rule then
      threshold := 4;
    else
      threshold := 5;
    end if;
    change := False;
    for i in 1 .. h loop
      for j in 1 .. w loop
        new_map (i, j) := current_map (i, j);
        case current_map (i, j) is
          when 'L' =>
            if Count_Visible_Occupied (i, j) = 0 then
              new_map (i, j) := '#';
              change := True;
            end if;
          when '#' =>
            if Count_Visible_Occupied (i, j) >= threshold then
              new_map (i, j) := 'L';
              change := True;
            end if;
          when others =>
            null;
        end case;
      end loop;
    end loop;
  end Move;
  --
  procedure Show (map : Map_Type) is
  begin
    for i in 1 .. h loop
      for j in 1 .. w loop
        Put (map (i, j));
      end loop;
      New_Line;
    end loop;
    Skip_Line;
  end Show;
  --
  function Count_Occupied (map : Map_Type) return Natural is
    occ : Natural := 0;
  begin
    for i in 1 .. h loop
      for j in 1 .. w loop
        if map (i, j) = '#' then
          occ := occ + 1;
        end if;
      end loop;
    end loop;
    return occ;
  end Count_Occupied;
  --
  f : File_Type;
  map_start : Map_Type;
  map : array (0 .. 1) of Map_Type;
  c : Natural := 0;
  changed : Boolean;
  verbose : constant Boolean := False;
  part : Positive := 1;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
begin
  Open (f, name);
  for i in 1 .. h loop
    for j in 1 .. w loop
      Get (f, map_start (i, j));
    end loop;
  end loop;
  Close (f);
  --
  for simple_rule in reverse Boolean loop
    map (c) := map_start;
    loop
      if verbose then Show (map (c)); end if;
      Move (map (c), map (1 - c), simple_rule, changed);
      exit when not changed;
      c := 1 - c;
    end loop;
    if compiler_test_mode then
      if Count_Occupied (map (c)) /= Integer_Value (Argument (part)) then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
    else
      Put_Line (
        +"Part " & part &
        ": stabilized at " & Count_Occupied (map (c)) & " occupied seats."
      );
    end if;
    part := part + 1;
  end loop;
  --  Part 1: Example: 37. Input (aoc_2020_11.txt): validated by AoC: 2386
  --  Part 2: Example: 26. Input (aoc_2020_11.txt): validated by AoC: 2091
end AoC_2020_11;
