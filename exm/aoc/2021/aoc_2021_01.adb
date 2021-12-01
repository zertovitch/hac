with HAL;  --  For "full Ada": files hal*.ad* are in ../../../src

procedure AoC_2021_01 is
  use HAL;
  f : File_Type;
  depth, depth_p1, depth_p2, inc_a, inc_b, row, sum, sum_p1 : Integer;
begin
  Open (f, "aoc_2021_01.txt");
  depth_p1 := -1;
  depth_p2 := -1;
  inc_a := 0;
  inc_b := 0;
  row := 0;
  sum := -1;
  while not End_Of_File (f) loop
    Get (f, depth);
    row := row + 1;
    if row > 1 and depth > depth_p1 then
      inc_a := inc_a + 1;
    end if;
    sum_p1 := sum;
    sum := depth + depth_p1 + depth_p2;
    if row > 3 and sum > sum_p1 then
      inc_b := inc_b + 1;
    end if;
    depth_p2 := depth_p1;
    depth_p1 := depth;
  end loop;
  Close (f);
  Put_Line (+"Part 1: Depth increases: " & inc_a);
  Put_Line (+"Part 2: Depth increases: " & inc_b);
end AoC_2021_01;
