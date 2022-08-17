--  Solution to Advent of Code 2020, Day 11
-------------------------------------------
--  Seating System
--
--  https://adventofcode.com/2020/day/11
--
--  Full Ada version.
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_11_full_Ada is

  procedure Solve (w, h : Positive; file_name : String) is
    --
    type Map_Type is array (1 .. h, 1 .. w) of Character;
    --
    empty    : constant Character := 'L';
    occupied : constant Character := '#';
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
              when occupied => occ := occ + 1; exit;
              when empty    => exit;
              when others   => exit when simple_rule;
            end case;
            ii := ii + di;
            jj := jj + dj;
          end loop;
        end Scan_Direction;
        --
      begin
        for di in -1 .. 1 loop
          for dj in -1 .. 1 loop
            if di /= 0 or else dj /= 0 then
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
            when empty =>
              if Count_Visible_Occupied (i, j) = 0 then
                new_map (i, j) := occupied;
                change := True;
              end if;
            when occupied =>
              if Count_Visible_Occupied (i, j) >= threshold then
                new_map (i, j) := empty;
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
          if map (i, j) = occupied then
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
  begin
    Put_Line (file_name);
    Open (f, file_name);
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
      Put_Line (
        +"  Part " & part &
        ": stabilized at " & Count_Occupied (map (c)) & " occupied seats."
      );
      part := part + 1;
    end loop;
    New_Line;
  end Solve;
  --
begin
  Solve (10, 10, "aoc_2020_11_mini.txt");
  Solve (93, 98, "aoc_2020_11.txt");
end AoC_2020_11_full_Ada;
