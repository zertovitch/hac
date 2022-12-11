--  Solution to Advent of Code 2022, Day 11
-------------------------------------------
--  Monkey in the Middle
--
--  https://adventofcode.com/2022/day/1
--  Copy of questions in: aoc_2022_11_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

--  Interfaces is needed for compiling on both
--  HAC and another compiler, like GNAT.
--  64-bit integer in Interfaces: Integer_64.
with Interfaces;

procedure AoC_2022_11 is
  use HAT, Interfaces;

  c, space : Character;
  idx, idx_comma : Integer;
  starting_items_string : String (1 .. 18);
  operation_string      : String (1 .. 23);
  test_string           : String (1 .. 21);
  monkey_true_string    : String (1 .. 28);
  monkey_false_string   : String (1 .. 29);
  i : Integer_64;
  f : File_Type;
  s : VString;

  subtype Monkey_Range is Integer range 0 .. 9;

  last_id : Integer;

  type Operation_Type is (plus, times, square);

  type Item_Worry_List is array (1 .. 100) of Integer_64;

  type Stack is record
    top       : Natural;
    s         : Item_Worry_List;
    operation : Operation_Type;
    operand   : Positive;
    divisor   : Positive;
    if_true   : Monkey_Range;
    if_false  : Monkey_Range;
    inspected : Natural;
  end record;

  mm : array (Monkey_Range) of Stack;

  dest : Monkey_Range;
  top, top_1, rounds : Positive;
  product_of_divisors : Integer_64;

  verbose : constant Boolean := False;
  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer_64;

begin
Parts :
  for part in 1 .. 2 loop
    case part is
      when 1 =>
        rounds := 20;
      when 2 =>
        rounds := 10_000;
    end case;
    product_of_divisors := 1;
    last_id := -1;
    Open (f, "aoc_2022_11.txt");
  Read_Data :
    loop
      last_id := last_id + 1;
      Skip_Line (f);
      mm (last_id).top := 0;
      mm (last_id).inspected := 0;
      Get (f, starting_items_string);
      Get_Line (f, s);
      loop
        idx_comma := Index (s, ',');
        if idx_comma > 0 then
          idx := idx_comma - 1;
        else
          idx := Length (s);
        end if;
        mm (last_id).top := mm (last_id).top + 1;
        mm (last_id).s (mm (last_id).top) :=
          Integer_64 (Integer_Value (Slice (s, 1, idx)));
        exit when idx_comma = 0;
        s := Slice (s, idx_comma + 1, Length (s));
      end loop;
      Get (f, operation_string);
      Get (f, c);
      case c is
        when '*' =>
          Get (f, space);
          Get (f, c);
          if c = 'o' then
            --  "old * old"
            mm (last_id).operation := square;
            Skip_Line (f);
          else
            mm (last_id).operation := times;
            Get_Line (f, s);
            mm (last_id).operand := Integer_Value (c & s);
          end if;
        when others =>
          mm (last_id).operation := plus;
          Get (f, mm (last_id).operand);
      end case;
      Get (f, test_string);
      Get (f, mm (last_id).divisor);
      product_of_divisors := product_of_divisors * Integer_64 (mm (last_id).divisor);
      Get (f, monkey_true_string);
      Get (f, mm (last_id).if_true);
      Get (f, monkey_false_string);
      Get (f, mm (last_id).if_false);
      exit when End_Of_File (f);
      Skip_Line (f, 2);
    end loop Read_Data;
    Close (f);

    for simulation in 1 .. rounds loop
      for m in 0 .. last_id loop
        if verbose then
          Put_Line (+"Monkey " & m);
        end if;
        for o in 1 .. mm (m).top loop
          if verbose then
            Put (+"  Worry:" & Integer_64'Image (mm (m).s (o)));
          end if;
          i := mm (m).s (o);
          case mm (m).operation is
            when plus =>
              i := i + Integer_64 (mm (m).operand);
              if verbose then
                Put (+"  Plus " & mm (m).operand);
              end if;
            when times =>
              i := i * Integer_64 (mm (m).operand);
              if verbose then
                 Put (+"  Times " & mm (m).operand);
              end if;
            when square =>
              i := i * i;
              if verbose then
                Put (+"  Square");
              end if;
          end case;
          case part is
            when 1 =>
              i := i / 3;
            when 2 =>
              i := i mod product_of_divisors;
              --  ^ i mod divisor = (i mod product_of_divisors) mod divisor.
          end case;
          if i mod Integer_64 (mm (m).divisor) = 0 then
            dest := mm (m).if_true;
          else
            dest := mm (m).if_false;
          end if;
          if verbose then
            Put_Line (+" throw" & Integer_64'Image (i) & " to " & dest);
          end if;
          mm (dest).top := mm (dest).top + 1;
          mm (dest).s (mm (dest).top) := i;
        end loop;
        mm (m).inspected := mm (m).inspected + mm (m).top;
        mm (m).top := 0;
      end loop;
    end loop;
    if verbose then
      Put_Line ("Inspected:");
      for m in 0 .. last_id loop
        Put_Line (mm (m).inspected);
      end loop;
    end if;
    top := 1;
    top_1 := 1;
    for m in 0 .. last_id loop
      if mm (m).inspected >= top then
        top_1 := top;
        top := mm (m).inspected;
      elsif mm (m).inspected > top_1 then
        top_1 := mm (m).inspected;
      end if;
    end loop;
    r (part) := Integer_64 (top) * Integer_64 (top_1);
    exit when compiler_test_mode;  --  We skip part 2 on compiler test.
  end loop Parts;

  if compiler_test_mode then
    if r (1) /= Integer_64'Value (To_String (Argument (1))) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Level of monkey business:");
    Put_Line (+"  part 1: " & Integer_64'Image (r (1)));
    Put_Line (+"  part 2: " & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 102399
    --  Part 2: validated by AoC: 23641658401
  end if;
end AoC_2022_11;
