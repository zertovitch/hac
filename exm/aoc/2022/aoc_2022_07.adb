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

  root : constant := -1;
  subtype Cat_Entry_Range is Integer range root .. 1000;

  type Dir_Info is record
    name   : VString;
    parent : Cat_Entry_Range;
    size   : Natural;
  end record;

  cat : array (Cat_Entry_Range) of Dir_Info;

  current, up, last, i_min : Cat_Entry_Range := root;

  size, new_disk_size,
  size_cumul_small_dirs, best_new_size : Natural := 0;

  f : File_Type;
  s : VString;

  verbose : constant Boolean := False;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

begin
  Open (f, "aoc_2022_07.txt");
  cat (root).name   := +"/";
  cat (root).parent := root;
  cat (root).size   := 0;
Read_Data :
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
                current := cat (current).parent;
              when '/' =>
                current := root;
              when others =>
                --  Subdirectory (hopefully unseen so far...):
                last := last + 1;
                cat (last).name :=
                  cat (current).name &
                  Slice (s, 6, Index (s, ' ', 6) - 1) & '/';
                cat (last).parent := current;
                cat (last).size   := 0;
                current := last;
            end case;
            if verbose then
              Put_Line (+"New dir " & cat (current).name);
            end if;
          when others => null;
        end case;

      when '0' .. '9' =>
        size := Integer_Value (Slice (s, 1, Index (s, ' ') - 1));
        if verbose then
          Put_Line (+"  File size : " & size);
        end if;
        up := current;
        loop
          cat (up).size := cat (up).size + size;
          if verbose then
            Put_Line
              (+"    New total size for " &
               cat (up).name & " : " & cat (up).size);
          end if;
          exit when up = root;
          up := cat (up).parent;
        end loop;

      when others =>
        --  Any other information, unused.
        null;
    end case;
  end loop Read_Data;
  Close (f);

  for i in root .. last loop
    --
    --  We sum up the size of all directories with total sizes up to 100_000.
    --  Sub-directories are double-counted.
    --
    if cat (i).size <= 100_000 then
      size_cumul_small_dirs := size_cumul_small_dirs + cat (i).size;
    end if;
    --
    --  We look for removing the smallest possible directory,
    --  which means getting the largest possible new disk size,
    --  as long as the new disk size is up to 40_000_000.
    --
    new_disk_size := cat (root).size - cat (i).size;
    if new_disk_size > best_new_size and then new_disk_size <= 40_000_000 then
      best_new_size := new_disk_size;
      i_min := i;
    end if;
  end loop;

  r (1) := size_cumul_small_dirs;
  r (2) := cat (i_min).size;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: sum of total sizes of at most 100_000 : " & r (1));
    Put_Line (+"Part 2: smallest directory to delete          : " & r (2));
    --  Part 1: validated by AoC: 1749646
    --  Part 2: validated by AoC: 1498966
  end if;
end AoC_2022_07;
