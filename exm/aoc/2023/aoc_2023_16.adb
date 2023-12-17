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

  type Direction is (No, Ea, So, We);

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
            Put ('.');
          when 1 =>
            for d in Direction loop
              if beam_map (i, j)(d) then
                case d is
                  when No => Put ('^');
                  when Ea => Put ('>');
                  when So => Put ('v');
                  when We => Put ('<');
                end case;
              end if;
            end loop;
          when 2 =>
            Put ('2');
          when 3 =>
            Put ('3');
          when 4 =>
            Put ('4');
          when others =>
            null;
        end case;
      end loop;
      New_Line;
    end loop;
  end Show_Beams;

  procedure Progress is
    procedure Offspring (i, j : Positive; d : Direction) is
    begin
      case d is
        when No =>
          if i > 1 then
            beam_map (i - 1, j)(d) := True;
          end if;
        when Ea =>
          if j < n then
            beam_map (i, j + 1)(d) := True;
          end if;
        when So =>
          if i < n then
            beam_map (i + 1, j)(d) := True;
          end if;
        when We =>
          if j > 1 then
            beam_map (i, j - 1)(d) := True;
          end if;
      end case;
    end Offspring;
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop
        for d in Direction loop
          if beam_map (i, j)(d) then
            case d is
              when No =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, Ea);
                  when '\' =>
                    Offspring (i, j, We);
                  when '-' =>  --  Flat side of a splitter
                    Offspring (i, j, Ea);
                    Offspring (i, j, We);
                  when others =>
                    Offspring (i, j, No);
                end case;
              when Ea =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, No);
                  when '\' =>
                    Offspring (i, j, So);
                  when '|' =>  --  Flat side of a splitter
                    Offspring (i, j, No);
                    Offspring (i, j, So);
                  when others =>
                    Offspring (i, j, Ea);
                end case;
              when So =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, We);
                  when '\' =>
                    Offspring (i, j, Ea);
                  when '-' =>  --  Flat side of a splitter
                    Offspring (i, j, Ea);
                    Offspring (i, j, We);
                  when others =>
                    Offspring (i, j, So);
                end case;
              when We =>
                case map (i, j) is
                  when '/' =>
                    Offspring (i, j, So);
                  when '\' =>
                    Offspring (i, j, No);
                  when '|' =>  --  Flat side of a splitter
                    Offspring (i, j, No);
                    Offspring (i, j, So);
                  when others =>
                    Offspring (i, j, We);
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
          end if;
        end loop;
        c := c + ct;
      end loop;
    end loop;
    return c;
  end Energized_Count;

  function Beam_Count return Natural is
    c : Natural := 0;
  begin
    for i in 1 .. n loop
      for j in 1 .. n loop
        for d in Direction loop
          if beam_map (i, j)(d) then
            c := c + 1;
          end if;
        end loop;
      end loop;
    end loop;
    return c;
  end Beam_Count;

  r : array (Part_Type) of Integer;

  verbose : constant Boolean := False;

  procedure Spread_Beams is
    bc, old_bc : Natural;
  begin
    old_bc := 0;
    loop
      Progress;
      bc := Beam_Count;
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
    beam_map (1, 1)(Ea) := True;
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
          when No => beam_map (n, x)(d) := True;
          when Ea => beam_map (x, 1)(d) := True;
          when So => beam_map (1, x)(d) := True;
          when We => beam_map (x, n)(d) := True;
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
    --  HAC is too slow on that one, which is basically 440x Part 1
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
