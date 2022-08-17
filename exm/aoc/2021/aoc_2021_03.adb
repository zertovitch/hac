--  Solution to Advent of Code 2021, Day 3
------------------------------------------
--  Binary Diagnostic
--
--  https://adventofcode.com/2021/day/3
--  Copy of questions in: aoc_2021_03_questions.txt
--
with HAT;
--  For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2021.gpr .

procedure AoC_2021_03 is
  use HAT;
  c : Character;
  f : File_Type;
  --
  input : constant VString := +"aoc_2021_03.txt";
  bits : constant := 12;
  subtype Bit_Pos_Range is Integer range 1 .. bits;
  stat_ones : array (Bit_Pos_Range) of Natural;
  type Criterium is (most, least);
  stat_ones_selected_pos : Natural;
  --
  subtype Bit_Type is Integer range 0 .. 1;
  type Word is array (Bit_Pos_Range) of Bit_Type;
  keep, bit_value, mem_valid : Word;
  --
  consider : Boolean;
  rows, gamma, epsilon, res_1, res_2 : Natural;
  gas : array (Criterium) of Natural;
  --
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := False;
begin
  for pos in Bit_Pos_Range loop
    stat_ones (pos) := 0;
  end loop;
  rows := 0;
  Open (f, input);
  while not End_Of_File (f) loop
    rows := rows + 1;
    for pos in Bit_Pos_Range loop
      Get (f, c);
      if c = '1' then
        stat_ones (pos) := stat_ones (pos) + 1;
      end if;
    end loop;
  end loop;
  Close (f);
  gamma := 0;
  for pos in Bit_Pos_Range loop
    gamma := gamma * 2;
    if stat_ones (pos) > rows / 2 then
      gamma := gamma + 1;
    end if;
  end loop;
  epsilon := 2 ** bits - 1 - gamma;
  if verbose then
    Put_Line (+"Part 1: gamma: " & gamma & "; epsilon: " & epsilon);
  end if;
  res_1 := gamma * epsilon;
  --
  --  Part Two
  --
  if verbose then Put_Line (+"Part 2:"); end if;
  for pos in Bit_Pos_Range loop
    keep (pos) := 0;  --  Just to remove a compiler warning.
  end loop;
  for crit in Criterium loop
    for main_pos in Bit_Pos_Range loop
      stat_ones_selected_pos  := 0;
      rows := 0;
      Open (f, input);
      while not End_Of_File (f) loop
        consider := True;
        for pos in Bit_Pos_Range loop
          Get (f, c);
          bit_value (pos) := Ord (c) - Ord ('0');
          if pos < main_pos then
            consider := consider and keep (pos) = bit_value (pos);
          elsif pos = main_pos then
            if consider then
              rows := rows + 1;
              if bit_value (pos) = 1 then
                stat_ones_selected_pos := stat_ones_selected_pos + 1;
              end if;
            end if;
          end if;
        end loop;
        if consider then
          mem_valid := bit_value;  --  Remember the number on the last considered row.
        end if;
      end loop;
      Close (f);
      if verbose then
         Put ("Bit pos: "); Put (main_pos, 2); Put (+"/" & bits & "; rows: "); Put (rows, 5);
      end if;
      if rows = 1 then
        keep := mem_valid;
        if verbose then
          Put_Line (". Only one row left, stop!");
        end if;
        exit;
      else
        case crit is
          when most =>
            if stat_ones_selected_pos >= rows - stat_ones_selected_pos then
              keep (main_pos) := 1;
            else
              keep (main_pos) := 0;
            end if;
          when least =>
            if stat_ones_selected_pos >= rows - stat_ones_selected_pos then
              keep (main_pos) := 0;
            else
              keep (main_pos) := 1;
            end if;
        end case;
        if verbose then
          Put (";  ones: "); Put (stat_ones_selected_pos, 5); Put (keep (main_pos)); New_Line;
        end if;
      end if;
    end loop;
    if verbose then
      for main_pos in Bit_Pos_Range loop
        Put (keep (main_pos), 0);
      end loop;
      New_Line;
    end if;
    gas (crit) := 0;
    for pos in Bit_Pos_Range loop
      gas (crit) := gas (crit) * 2;
      if keep (pos) = 1 then
        gas (crit) := gas (crit) + 1;
      end if;
    end loop;
  end loop;
  if verbose then
    Put_Line (+"Oxygen: " & gas (most));
    Put_Line (+"CO2: " & gas (least));
  end if;
  res_2 := gas (most) * gas (least);
  --
  if compiler_test_mode then
    if res_1 /= Integer_Value (Argument (1)) or
       res_2 /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Result Part 1: " & res_1);
    Put_Line (+"Result Part 2: " & res_2);
    --  Part 1: validated by AoC: 3549854
    --  Part 2: validated by AoC: 3765399
  end if;
end AoC_2021_03;
