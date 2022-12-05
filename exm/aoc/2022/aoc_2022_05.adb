--  Solution to Advent of Code 2022, Day 5
------------------------------------------
--  Supply Stacks
--
--  https://adventofcode.com/2022/day/5
--  Copy of questions in: aoc_2022_05_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_05 is
  use HAT;

  type Storage is array (1 .. 100) of Character;

  type Stack is record
    top : Natural;
    s   : Storage;
  end record;

  s : array (1 .. 9) of Stack;

  procedure Show (title : VString) is
    empty : Boolean;
  begin
    Put_Line (title);
    New_Line;
    for y in reverse 1 .. Storage'Last loop
      empty := True;
      for x in s'Range loop
        empty := empty and y > s (x).top;
      end loop;
      if not empty then
        for x in s'Range loop
          if y > s (x).top then
            Put ("    ");
          else
            Put (+'[' & s (x).s (y) & "] ");
          end if;
        end loop;
        New_Line;
      end if;
    end loop;
    Put_Line ((4 * s'Length - 1) * '-');
    New_Line;
  end Show;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of String (1 .. 9);

  c, sep, bra, ket : Character;
  move : String (1 .. 4);
  from : String (1 .. 5);
  to   : String (1 .. 4);
  a, b, n : Integer;
  max_y : Natural := 0;
  name : constant VString := +"aoc_2022_05.txt";
  f : File_Type;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant Boolean := not compiler_test_mode;

begin
  --  Get the maximum height of the crates stacks:
  Open (f, name);
  while not End_Of_File (f) loop
    Get (f, sep);
    Get (f, c);
    exit when c in '1' .. '9';
    max_y := max_y + 1;
    Skip_Line (f);
  end loop;
  Close (f);
  --
  for part in 1 .. 2 loop
    for x in s'Range loop
      s (x).top := 0;
    end loop;
    --  Get the initial setup of the crates:
    Open (f, name);
    for y in reverse 1 .. max_y loop
      for x in s'Range loop
        Get (f, bra);
        Get (f, c);
        if c in 'A' .. 'Z' then
          s (x).s (y) := c;
          s (x).top := Max (s (x).top, y);
        end if;
        Get (f, ket);
        if x < s'Last then
          Get (f, sep);
        end if;
      end loop;
    end loop;
    if verbose and part = 1 then
      Show (+"Data:");
    end if;
    --  Skip the "1 2 3 ..." line and the next empty one:
    Skip_Line (f, 2);
    --  Read and execute the instructions for moving crates:
    while not End_Of_File (f) loop
      --  Read line "move n from a to b":
      Get (f, move);
      Get (f, n);
      Get (f, from);
      Get (f, a);
      Get (f, to);
      Get (f, b);
      for count in 1 .. n loop
        case part is
          when 1 =>
            --  Move one by one, taking each crate `c` from the top, downwards:
            c := s (a).s (s (a).top - count + 1);
          when 2 =>
            --  Move the crates "stack-wise":
            c := s (a).s (s (a).top - n + count);
        end case;
        s (b).s (s (b).top + count) := c;
      end loop;
      s (a).top := s (a).top - n;
      s (b).top := s (b).top + n;
    end loop;
    Close (f);
    if verbose then
      Show (+"Result, part " & part & ':');
    end if;
    for x in s'Range loop
      r (part)(x) := s (x).s (s (x).top);
    end loop;
  end loop;
  if compiler_test_mode then
    if +r (1) /= Argument (1) or +r (2) /= Argument (2) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Top crates after...");
    Put_Line (+" (part 1) one-by-one moves: " & r (1));
    Put_Line (+" (part 2) stack-wise moves: " & r (2));
    --  Part 1: validated by AoC: VQZNJMWTR
    --  Part 2: validated by AoC: NLCDCLVMQ
  end if;
end AoC_2022_05;
