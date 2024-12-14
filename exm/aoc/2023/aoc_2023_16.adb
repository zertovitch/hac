--  Solution to Advent of Code 2023, Day 16
-------------------------------------------
--  The Floor Will Be Lava
--
--  https://adventofcode.com/2023/day/16
--  Copy of questions in: aoc_2023_16_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in the upper directory (..)
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_16 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini"; n : constant := 10;
  input_name : constant VString := +"aoc_2023_16"; n : constant := 110;

  map : array (1 .. n, 1 .. n) of Character;

  type Tile_Beam is array (Direction) of Boolean;

  clean_tile : Tile_Beam;

  type Beam_Map_Type is array (1 .. n, 1 .. n) of Tile_Beam;

  beam_map, clean_beam_map : Beam_Map_Type;

  procedure Read_Data is
    f : File_Type;
  begin
    for d in Direction loop
      clean_tile (d) := False;
    end loop;
    Open (f, input_name & ".txt");
    for i in 1 .. n loop
      for j in 1 .. n loop
        Get (f, map (i, j));
        clean_beam_map (i, j) := clean_tile;
      end loop;
    end loop;
    Close (f);
  end Read_Data;

  procedure Show_Beams is
    ct : Natural;
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop
        ct := 0;
        for d in Direction loop
          if beam_map (i, j)(d) then
            ct := ct + 1;
          end if;
        end loop;
        case ct is
          when 0 =>
            Put ('.');  --  No beam
          when 1 =>
            for d in Direction loop
              if beam_map (i, j)(d) then
                case d is
                  when north => Put ('^');
                  when east  => Put ('>');
                  when south => Put ('v');
                  when west  => Put ('<');
                end case;
              end if;
            end loop;
          when 2 =>
            Put ('2');  --  Beams in two directions
          when 3 =>
            Put ('3');  --  Beams in three directions
          when 4 =>
            Put ('4');  --  Beams in four directions
          when others =>
            null;
        end case;
      end loop;
      New_Line;
    end loop;
  end Show_Beams;

  procedure Progress (partial_state_beam_count : out Natural) is

    procedure Offspring (i, j : Positive; d : Direction) is
    begin
      case d is
        when north =>
          if i > 1 then
            beam_map (i - 1, j)(d) := True;
          end if;
        when east =>
          if j < n then
            beam_map (i, j + 1)(d) := True;
          end if;
        when south =>
          if i < n then
            beam_map (i + 1, j)(d) := True;
          end if;
        when west =>
          if j > 1 then
            beam_map (i, j - 1)(d) := True;
          end if;
      end case;
    end Offspring;

  begin
    partial_state_beam_count := 0;
    for i in 1 .. n loop
      for j in 1 .. n loop
        for d in Direction loop
          --  There can be up to four different
          --  beams through cell (i, j).
          if beam_map (i, j)(d) then
            partial_state_beam_count := partial_state_beam_count + 1;
            --  ^ This count will miss propagation of beams onto cells
            --    on a row < i or a column < j.
            --    But on next call to `Progress`, they will be in.
            case d is
              when north =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, east);
                  when '\' =>
                    Offspring (i, j, west);
                  when '-' =>  --  Flat side of a splitter
                    Offspring (i, j, east);
                    Offspring (i, j, west);
                  when others =>
                    Offspring (i, j, north);
                end case;
              when east =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, north);
                  when '\' =>
                    Offspring (i, j, south);
                  when '|' =>  --  Flat side of a splitter
                    Offspring (i, j, north);
                    Offspring (i, j, south);
                  when others =>
                    Offspring (i, j, east);
                end case;
              when south =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, west);
                  when '\' =>
                    Offspring (i, j, east);
                  when '-' =>  --  Flat side of a splitter
                    Offspring (i, j, east);
                    Offspring (i, j, west);
                  when others =>
                    Offspring (i, j, south);
                end case;
              when west =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, south);
                  when '\' =>
                    Offspring (i, j, north);
                  when '|' =>  --  Flat side of a splitter
                    Offspring (i, j, north);
                    Offspring (i, j, south);
                  when others =>
                    Offspring (i, j, west);
                end case;
            end case;
          end if;
        end loop;
      end loop;
    end loop;
  end Progress;

  function Energized_Count return Natural is
    c : Natural := 0;
    ct : Natural;
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop
        ct := 0;
        for d in Direction loop
          if beam_map (i, j)(d) then
            ct := 1;
            exit;
          end if;
        end loop;
        c := c + ct;
      end loop;
    end loop;
    return c;
  end Energized_Count;

  r : array (Part_Type) of Integer;

  verbose : constant Boolean := False;

  procedure Spread_Beams is
    bc, old_bc : Natural;
  begin
    old_bc := 0;
    loop
      Progress (bc);
      exit when old_bc = bc;
      old_bc := bc;
    end loop;
    if verbose then
      Show_Beams;
    end if;
  end Spread_Beams;

  procedure Do_Part_1 is
  begin
    beam_map := clean_beam_map;
    beam_map (1, 1)(east) := True;
    Spread_Beams;
    r (part_1) := Energized_Count;
  end Do_Part_1;

  procedure Do_Part_2 is
  begin
    r (part_2) := 0;
    for x in 1 .. n loop
      for d in Direction loop
        beam_map := clean_beam_map;
        case d is
          when north => beam_map (n, x)(d) := True;
          when east  => beam_map (x, 1)(d) := True;
          when south => beam_map (1, x)(d) := True;
          when west  => beam_map (x, n)(d) := True;
        end case;
        Spread_Beams;
        r (part_2) := Max (r (part_2), Energized_Count);
      end loop;
    end loop;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;
  if not compiler_test_mode then
    --  HAC is too slow on Part 2, which is basically 440x Part 1,
    --  with different entrance points and directions.
    --
    --  GNAT (fast mode) takes 7 seconds.
    --  HAC (VM) takes an estimated 7920 seconds, or 2.2 hours
    --  that is, a factor 1131 !
    Do_Part_2;
  end if;
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 7996
    --  Part 2: validated by AoC: 8239
  end if;
end AoC_2023_16;
