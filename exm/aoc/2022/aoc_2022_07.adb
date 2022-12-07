--  Solution to Advent of Code 2022, Day 7
------------------------------------------
--  No Space Left On Device
--
--  https://adventofcode.com/2022/day/7
--  Copy of questions in: aoc_2022_07_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_07 is
  use HAT;

  type Dir_Info is record
    name   : VString;
    parent : Natural;
    size   : Natural;
  end record;

  subtype Cat_Entry_Range is Integer range 0 .. 1000;

  cat : array (Cat_Entry_Range) of Dir_Info;

  cur, up, tot, i_min : Cat_Entry_Range := 0;

  size, size_cumul_small_dirs, best_new_size, new_size : Natural;

  root : constant := 0;

  verbose : constant Boolean := False;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

  f : File_Type;
  s : VString;

begin
  Open (f, "aoc_2022_07.txt");
  cat (root).name   := +"/";
  cat (root).parent := 0;
  cat (root).size   := 0;
  while not End_Of_File (f) loop
    Get_Line (f, s);
    s := s & ' ';
    case Element (s, 1) is
      when '$' =>
        --  Command:
        case Element (s, 3) is
          when 'c' =>
            --  "cd":
            case Element (s, 6) is
              when '.' =>
                cur := cat (cur).parent;
              when '/' =>
                cur := root;
              when others =>
                --  Subdirectory (hopefully new...):
                tot := tot + 1;
                cat (tot).name :=
                  cat (cur).name & Slice (s, 6, Index (s, ' ', 6) - 1) & '/';
                cat (tot).parent := cur;
                cur := tot;
            end case;
            if verbose then
              Put_Line (+"New dir " & cat (cur).name);
            end if;
          when others => null;
        end case;
      when '0' .. '9' =>
        size := Integer_Value (Slice (s, 1, Index (s, ' ') - 1));
        if verbose then
          Put_Line (+" file size -->" & size);
        end if;
        up := cur;
        loop
          cat (up).size := cat (up).size + size;
          if verbose then
            Put_Line
              (+"  new total size -> " & cat (up).name & ": " & cat (up).size);
          end if;
          exit when up = 0;
          up := cat (up).parent;
        end loop;
      when others =>
        --  Any other information, unused.
        null;
    end case;
  end loop;
  Close (f);
  size_cumul_small_dirs := 0;
  best_new_size := 0;
  i_min := 0;
  for i in 0 .. tot loop
    if cat (i).size <= 100_000 then
      size_cumul_small_dirs := size_cumul_small_dirs + cat (i).size;
    end if;
    new_size := cat (0).size - cat (i).size;
    if new_size > best_new_size and then new_size <= 40_000_000 then
      best_new_size := new_size;
      i_min := i;
    end if;
  end loop;
  r (1) := size_cumul_small_dirs;
  r (2) := cat (i_min).size;
  --
  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: sum of total sizes of at most 100_000 : " & Image (r (1)));
    Put_Line (+"Part 2: smallest directory to delete          : " & Image (r (2)));
    --  Part 1: validated by AoC: 1749646
    --  Part 2: validated by AoC: 1498966
  end if;
end AoC_2022_07;
