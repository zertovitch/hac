--  Solution to Advent of Code 2025, Day 10
-------------------------------------------
--  Factory
--
--  https://adventofcode.com/2025/day/10
--  Copy of questions in: aoc_2025_10_questions.txt
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

with Interfaces;

procedure AoC_2025_10 is

  use AoC_Toolbox, HAT, Interfaces;
  input_name : VString;

  max_lights : constant := 10;
  max_buttons : constant := 20;

  r : array (Part_Type) of VString;

  verbose : constant Boolean := False;
  do_part_2 : constant Boolean := False;

  procedure Read_Data is
    c : Character;
    light, lights : Integer;
    f : File_Type;
    buttons : Natural;
    diagram, test : Integer_64;
    button_bits : array (1 .. max_buttons) of Integer_64;  --  Light is 0-based.
    button : array (1 .. max_buttons, 1 .. max_lights) of Boolean;  --  All 1-based.
    fewest_presses, presses, res_1, res_2 : Natural;

    type Press_Combination is array (1 .. max_buttons) of Natural;
    type Counters is array (1 .. max_lights) of Natural;

    target : Counters;

    procedure Explore (press : Press_Combination; joltage : Counters; shift : Natural) is
      ok : Boolean := True;
      new_button : Integer;
      new_press : Press_Combination;
      new_joltage : Counters;
    begin
      if verbose then
        Put ("Presses: ");
        for b in 1 .. buttons loop
          Put (press (b), 2);
        end loop;
        Put (". Jolts: ");
        for l in 1 .. lights loop
          Put (joltage (l), 3);
        end loop;
        Skip_Line;
      end if;

      for l in 1 .. lights loop
        ok := ok and joltage (l) = target (l);
        if joltage (l) > target (l) then
          return;
        end if;
      end loop;
      if ok then
        --  This combination of buttons makes the joltages match.
        presses := 0;
        for b in 1 .. buttons loop
          presses := presses + press (b);
        end loop;
        fewest_presses := Min (presses, fewest_presses);
        return;
      end if;

      --  From here we know we have an incomplete joltage.
      --  We try something: press each button individually.
      for b in 1 .. buttons loop
        new_button := 1 + (b + shift - 1) mod buttons;
        for pow in 0 .. 0 loop
          new_press := press;
          new_joltage := joltage;
          new_press (new_button) := new_press (new_button) + 2 ** pow;
          for l in 1 .. lights loop
            if button (new_button, l) then
              new_joltage (l) := new_joltage (l) + 2 ** pow;
            end if;
          end loop;
          Explore (new_press, new_joltage, shift + 1);
        end loop;
      end loop;
    end Explore;

    press_0 : Press_Combination;
    joltage_0 : Counters;

  begin
    res_1 := 0;
    res_2 := 0;
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop

      light := -1;
      Get (f, c);  -- [
      diagram := 0;
      loop
        Get (f, c);
        exit when c = ']';
        light := light + 1;
        if c = '#' then
          diagram := diagram + 2 ** light;
        end if;
      end loop;
      lights := light + 1;

      buttons := 0;
      loop
        Get (f, c);
        case c is
          when '(' =>
            buttons := buttons + 1;
            button_bits (buttons) := 0;
            for l in 1 .. max_lights loop
              button (buttons, l) := False;
            end loop;
            --  Put ('b');
            --  Put (buttons,0);
            --  Put ('(');
          when ')' =>
            null;
            --  Put (')');
          when '{' =>
            exit;
          when '0' .. '9' =>
            light := Ord (c) - Ord ('0');
            button_bits (buttons) := button_bits (buttons) + 2 ** light;
            button (buttons, light + 1) := True;
            --  Put (button (buttons, l) - 1, 2);
          when others =>
            null;
        end case;
      end loop;

      if verbose then
        for b in 1 .. buttons loop
          for l in 1 .. lights loop
            if button (b, l) then
              Put ("1 ");
            else
              Put ("0 ");
            end if;
          end loop;
          New_Line;
        end loop;
      end if;

      for l in 1 .. lights loop
        Get (f, target (l));
        Get (f, c);
      end loop;
      if c /= '}' then
        Put_Line ("Data error on joltage");
      end if;

      --  Puzzle 1, light diagram.
      --  The order of the button presses is not relevant, and two presses are equivalent to zero presses.

      fewest_presses := buttons;
      --  !!  HAC borks on that : "for n in Integer_64 range 0 .. 2 ** buttons - 1 loop"
      for n in 0 .. 2 ** buttons - 1 loop
        --  Test all button combinations.
        test := 0;
        presses := 0;
        for b in 1 .. buttons loop
          if Sim_AND (2 ** (b - 1), Integer_64 (n)) /= 0 then
            presses := presses + 1;
            test := Sim_XOR (test, button_bits (b));
          end if;
        end loop;
        if test = diagram then
          --  This combination of buttons makes the light diagram match.
          fewest_presses := Min (presses, fewest_presses);
        end if;
      end loop;
      res_1 := res_1 + fewest_presses;

      --  Puzzle 2, joltages.
      --  It is actually an underdetermined linear equations system
      --  with integer solutions.
      
      if do_part_2 then 
        for b in 1 .. buttons loop
          press_0 (b) := 0;
        end loop;
        for l in 1 .. lights loop
          joltage_0 (l) := 0;
        end loop;
        fewest_presses := Integer'Last;
        Explore (press_0, joltage_0, 0);
        res_2 := res_2 + fewest_presses;
      end if;

    end loop;
    Close (f);
    r (part_1) := +"" & res_1;
    r (part_2) := +"" & res_2;
  end Read_Data;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  input_name := +"aoc_2025_10_mini";

  Read_Data;

  if compiler_test_mode then
    if r (part_1) /= Argument (1) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    if do_part_2 then
      Put_Line (+"Part 2: " & r (part_2));
    end if;
    --  Part 1: validated by AoC: 477 (mini: 7).
    --  Part 2: validated by AoC: .
  end if;
end AoC_2025_10;
