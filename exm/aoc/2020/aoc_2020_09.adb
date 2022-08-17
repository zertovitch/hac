--  Solution to Advent of Code 2020, Day 9
------------------------------------------
--  Encoding Error
--
--  https://adventofcode.com/2020/day/9
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_09 is
  contig_max, last_data, n, min, max,
  x, t : Integer;
  res_no_pair, weakness : Integer := 0;
  f : File_Type;
  mem_max  : constant := 24;
  subtype Mem_Range is Integer range 0 .. mem_max;
  mem : array (Mem_Range) of Integer;
  found : Boolean;
  test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  for part in 1 .. 2 loop
    Open (f, "aoc_2020_09.txt");
    for i in Mem_Range loop
      Get (f, mem (i));
    end loop;
    n := 0;
    while not End_Of_File (f) loop
      Get (f, last_data);
      found := False;
      if part = 1 then
        --  Find a pair of numbers in the recent data whose
        --  sum is the number read just now (last_data).
        for j in Mem_Range loop
          for k in j + 1 .. mem_max loop
            if last_data = mem (j) + mem (k) then
              found := True;
              exit;
            end if;
          end loop;
          exit when found;
        end loop;
        if not found then
          res_no_pair := last_data;
          exit;
        end if;
      else
        --  Find a contiguous set of at least two numbers in the list
        --  which sum to the invalid number from part 1.
        contig_max := n + mem_max;
        for j in n .. contig_max loop
          for k in j + 1 .. contig_max loop
            t := 0;
            min := mem (j mod (mem_max + 1));
            max := min;
            for i in j .. k loop
              x := mem (i mod (mem_max + 1));
              t := t + x;
              if min > x then min := x; end if;
              if max < x then max := x; end if;
            end loop;
            if t = res_no_pair then
              weakness := min + max;
              found := True;
              exit;
            end if;
          end loop;
          exit when found;
        end loop;
        exit when found;
      end if;
      mem (n) := last_data;
      n := (n + 1) mod (mem_max + 1);
    end loop;
    Close (f);
  end loop;
  if test_mode then
    if res_no_pair /= Integer_Value (Argument (1)) or
       weakness /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:  Not a sum of a pair: " & res_no_pair);
    Put_Line (+"Part 2:  Encryption weakness: " & weakness);
    --  Part 1: officially validated by AoC: 138879426
    --  Part 2: officially validated by AoC: 23761694
  end if;
end AoC_2020_09;
