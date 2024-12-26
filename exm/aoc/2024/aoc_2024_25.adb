--  Solution to Advent of Code 2024, Day 25
-------------------------------------------
--  Code Chronicle
--
--  https://adventofcode.com/2024/day/25
--  Copy of questions in: aoc_2024_25_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

procedure AoC_2024_25 is

  use AoC_Toolbox, HAT;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2024_25";

  verbose : constant Boolean := False;

  subtype Pin_Range is Integer range 1 .. 5;

  type Pin_Height is array (Pin_Range) of Natural;

  key, lock : array (1 .. 500) of Pin_Height;

  last_key, last_lock : Natural := 0;

  r : array (Part_Type) of VString;

  procedure Read_Data is
    subtype Pattern is String (Pin_Range);
    pat : Pattern;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    Get (f, pat);
    while not End_Of_File (f) loop
      exit when pat = "STOP.";
      if pat = "#####" then
        --  Lock
        last_lock := last_lock + 1;
        for i in 0 .. 6 loop
          for j in Pin_Range loop
            if pat (j) = '#' then
              lock (last_lock)(j) := i;
            end if;
          end loop;
          Get (f, pat);
        end loop;
        if verbose then
          Put ("Lock ");
          for j in Pin_Range loop
            Put (lock (last_lock)(j), 0);
          end loop;
          New_Line;
        end if;
      else
        --  Key
        last_key := last_key + 1;
        for j in Pin_Range loop
          key (last_key)(j) := 0;
        end loop;
        for i in 0 .. 6 loop
          for j in Pin_Range loop
            if pat (j) = '#' and then key (last_key)(j) = 0 then
              key (last_key)(j) := 6 - i;
            end if;
          end loop;
          Get (f, pat);
        end loop;
        if verbose then
          Put ("Key ");
          for j in Pin_Range loop
            Put (key (last_key)(j), 0);
          end loop;
          New_Line;
        end if;
      end if;
    end loop;
    Close (f);
  end Read_Data;

  procedure Do_Part_1 is
    count : Natural := 0;
    ok : Boolean;
  begin
    for k in 1 .. last_key loop
      for l in 1 .. last_lock loop
        ok := True;
        for j in Pin_Range loop
          ok := ok and then key (k)(j) + lock (l)(j) <= 5;
        end loop;
        if ok then
          count := count + 1;
        end if;
      end loop;
    end loop;
    r (part_1) := +"" & count;
  end Do_Part_1;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  Read_Data;
  Do_Part_1;

  if compiler_test_mode then if r (part_1) /= Argument (1) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    --  Part 1: validated by AoC: 3395
  end if;
end AoC_2024_25;
