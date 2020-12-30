--  Solution to Advent of Code 2020, Day 25
-------------------------------------------
--  Combo Breaker
--
--  https://adventofcode.com/2020/day/25
--
with HAC_Pack;  use HAC_Pack;  --  in ../../../src

procedure AoC_2020_25 is

  N : constant := 20201227;  --  A prime number.

  function Transform (subjet_number, loop_size : Positive) return Natural is
    value : Integer := 1;
  begin
    for i in 1 .. loop_size loop
      value := value * subjet_number;
      value := value rem N;
    end loop;
    return value;
  end Transform;

  procedure Solve (seed, card_public_key, door_public_key : Positive) is

    function Find_Loop_Size (key : Natural) return Natural is
      value : Integer := 1;
      max_loop_size : constant := 10_000_000;
    begin
      for i in 1 .. max_loop_size loop
        value := value * seed;
        value := value rem N;
        if key = value then
          return i;
        end if;
      end loop;
      return 0;
    end Find_Loop_Size;

    card_loop_size : constant Natural := Find_Loop_Size (card_public_key);
  begin
    Put_Line (+"  Card's reverse-engineered loop size : " & card_loop_size);
    Put_Line (+"  Door's reverse-engineered loop size : " & Find_Loop_Size (door_public_key));
    --
    Put_Line (+"  Encryption key : " & Transform (door_public_key, card_loop_size));
    --
    --  NB:
    --
    --   card_public_key ** door_loop_size         =
    --  (seed ** card_loop_size) ** door_loop_size =
    --   seed ** (card_loop_size * door_loop_size) =
    --  (seed ** door_loop_size) ** card_loop_size =
    --   door_public_key ** card_loop_size
    --
  end Solve;

  procedure Test_Example is
    card_public_key : constant Natural := Transform (7, 8);
    door_public_key : constant Natural := Transform (7, 11);
  begin
    Put_Line ("Example");
    Put_Line (+"  Card's public key : " & card_public_key);
    Put_Line (+"  Door's public key : " & door_public_key);
    --
    Solve (7, card_public_key, door_public_key);
  end Test_Example;

  procedure Input is
  begin
    Put_Line ("Input");
    Solve (7, 18356117, 5909654);
    --  Officially validated by AoC: 16902792
  end Input;

begin
  Test_Example;
  Input;
end AoC_2020_25;
