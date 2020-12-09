--  Solution to Advent of Code 2020, Day 9
------------------------------------------
--  Encoding Error
--
--  https://adventofcode.com/2020/day/9
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_09 is
  contig_max, last_data, n, min, max, x, t, res_no_pair : Integer;
  f : File_Type;
  mem_size : constant := 25;
  mem_max  : constant := 24;
  subtype Mem_Range is Integer range 0 .. mem_max;
  mem : array (Mem_Range) of Integer;
  ok, found : Boolean;
begin
  for part in 1 .. 2 loop
    Put (+"Part: " & part);
    Open (f, "aoc_2020_09.txt");
    for i in Mem_Range loop
      Get (f, mem (i));
    end loop;
    n := 0;
    while not End_Of_File (f) loop
      Get (f, last_data);
      if part = 1 then
        ok := False;
        for j in Mem_Range loop
          for k in j + 1 .. mem_max loop
            if last_data = mem (j) + mem (k) then
              ok := True;
              exit;
            end if;
          end loop;
          exit when ok;
        end loop;
        if not ok then
          res_no_pair := last_data;
          Put_Line (+" ->  Not a sum of a pair: " & res_no_pair);  --  138879426
          exit;
        end if;
      else
        found := False;
        contig_max := n + mem_max;
        for j in n .. contig_max loop
          for k in j + 1 .. contig_max loop
            t := 0;
            min := mem (j mod mem_size);
            max := min;
            for i in j .. k loop
              x := mem (i mod mem_size);
              t := t + x;
              if min > x then min := x; end if;
              if max < x then max := x; end if;
            end loop;
            if t = res_no_pair then
              Put_Line (+" ->  Encryption weakness: " & (min + max));  --  23761694
              found := True;
              exit;
            end if;
          end loop;
          exit when found;
        end loop;
        exit when found;
      end if;
      mem (n) := last_data;
      n := (n + 1) mod mem_size;
    end loop;
    Close (f);
  end loop;
end AoC_2020_09;
