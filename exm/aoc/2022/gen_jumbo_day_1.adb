--  Translation of gen_jumbo_day1.py:
--
--  import random
--  random.seed(42)
--
--  with open('day1_jumbo', 'w') as fd:
--      for i in range(10_000_000):
--          for j in range(random.randint(3, 10)):
--              fd.write(str(random.randint(1, 8) * 1000) + '\n')
--          fd.write('\n')

with HAT;

procedure Gen_Jumbo_Day_1 is
  use HAT;
  fd : File_Type;
begin
  Random_Seed (42);
  Create (fd, "day1_jumbo.txt");
  for i in 1 .. 10_000_000 loop
    for j in 1 .. 3 + Rand (7) loop
      Put_Line (fd, (1 +  Rand (7)) * 1000, 0);
    end loop;
    New_Line (fd);
  end loop;
  Close (fd);
end Gen_Jumbo_Day_1;
