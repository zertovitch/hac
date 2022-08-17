--  Solution to Advent of Code 2020, Day 17
-------------------------------------------
--  Conway Cubes
--
--  https://adventofcode.com/2020/day/17
--
--  NB: Was able to recycle parts from another "Game of Life"
--      puzzle: Seating System (Day 11).
--
--  Run time with GNAT (use the aoc_2020.gpr project file,
--          AoC_Build_Mode = "Fast", or compile with
--          gnatmake -O3 -gnatpn -I../../../src aoc_2020_17):
--    *  0.18 seconds on a i5-9400 @ 2.9 GHz
--
--  Run time with HAC (Virtual Machine + no compilation optimization:
--          we expect a big slowdown with this problem (many nested loops):
--    *  (too many) seconds, on a i5-9400 @ 2.9 GHz
--
--  HAC 0.084 "nice to have"'s detected in this exercise:
--
--    *     ` map (0) := (others => (others =>  (others => (others => False)); `
--
with HAT; use HAT;  --  For a build with "full Ada": files HAT*.ad* are in ../../../src

procedure AoC_2020_17 is
  --
  max : constant :=  10;
  min : constant := -max;
  subtype R is Integer range min .. max;
  type State is (Inactive, Active);
  type Map_Type is array (R, R, R, R) of State;
  --
  procedure Move (
    current_map : in     Map_Type;
    new_map     :    out Map_Type;
    wmin, wmax  : in     Integer    --  Relative and absolute range in 4th dimension
  )
  is
    dl_max : Natural;
    function Count_Visible_Occupied (i, j, k, l : Integer) return Natural is
      occ : Natural := 0;
      --
      procedure Scan_Direction (di, dj, dk, dl : Integer) is
        ii : constant Integer := i + di;
        jj : constant Integer := j + dj;
        kk : constant Integer := k + dk;
        ll : constant Integer := l + dl;
      begin
        loop
          exit when ii not in R;
          exit when jj not in R;
          exit when kk not in R;
          exit when ll not in R;
          case current_map (ii, jj, kk, ll) is
            when Active   => occ := occ + 1; exit;
            when Inactive => exit;
          end case;
        end loop;
      end Scan_Direction;
      --
    begin
      for di in -1 .. 1 loop
        for dj in -1 .. 1 loop
          for dk in -1 .. 1 loop
            for dl in -dl_max .. dl_max loop
              if di /= 0 or else dj /= 0 or else dk /= 0 or else dl /= 0 then
                Scan_Direction (di, dj, dk, dl);
              end if;
            end loop;
          end loop;
        end loop;
      end loop;
      return occ;
    end Count_Visible_Occupied;
    --
    occ : Natural;
  begin
    if wmax = 0 then
      dl_max := 0;
    else
      dl_max := 1;
    end if;
    for i in R loop
      for j in R loop
        for k in R loop
          for l in wmin .. wmax loop
            new_map (i, j, k, l) := current_map (i, j, k, l);
            occ := Count_Visible_Occupied (i, j, k, l);
            case current_map (i, j, k, l) is
              when Active =>
                if occ < 2 or else occ > 3 then
                  new_map (i, j, k, l) := Inactive;
                end if;
              when Inactive =>
                if occ = 3 then
                  new_map (i, j, k, l) := Active;
                end if;
            end case;
          end loop;
        end loop;
      end loop;
    end loop;
  end Move;
  --
  function Count_Occupied (map : Map_Type; wmin, wmax : Integer) return Natural is
    occ : Natural := 0;
  begin
    for i in R loop
      for j in R loop
        for k in R loop
          for l in wmin .. wmax loop
            if map (i, j, k, l) = Active then
              occ := occ + 1;
            end if;
          end loop;
        end loop;
      end loop;
    end loop;
    return occ;
  end Count_Occupied;
  --
  map : array (0 .. 1) of Map_Type;
  c : Natural := 0;
  cc : Character;
  f : File_Type;
  size : Integer;
  cycles : constant := 6;
  low, high : Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  wmin, wmax : Integer;
begin
  for dim_4 in Boolean loop
    if dim_4 then
      wmin := min;
      wmax := max;
    else
      wmin := 0;
      wmax := 0;
    end if;
    --
    for x in R loop
      for y in R loop
        for z in R loop
          for w in wmin .. wmax loop
            map (0)(x, y, z, w) := Inactive;
          end loop;
        end loop;
      end loop;
    end loop;
    --
    Open (f, "aoc_2020_17.txt");
    size := 8;
    low := -size / 2;
    high := low + size - 1;
    if low - cycles < min or high + cycles > max then
      Put_Line ("Test (hyper)space is too small");
      return;
    end if;
    for x in low .. high loop
      for y in low .. high loop
        Get (f, cc);
        if cc = '#' then
           map (0)(x, y, 0, 0) := Active;
        end if;
      end loop;
    end loop;
    Close (f);
    for cy in 1 .. cycles loop
      Move (map (c), map (1 - c), wmin, wmax);
      c := 1 - c;
    end loop;
    --
    if compiler_test_mode then
      if Count_Occupied (map (c), wmin, wmax) /= Integer_Value (Argument (1))
      then
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
      exit;
      --  ^ This is for HAC & compiler testing: we skip part 2, takes too long.
    else
      if dim_4 then
        Put ("Part 2, 4");
      else
        Put ("Part 1, 3");
      end if;
      Put (
        +"-dimensional: number of active cells is " &
        Count_Occupied (map (c), wmin, wmax));
      New_Line;
    end if;
  end loop;
end AoC_2020_17;
