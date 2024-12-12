--  Solution to Advent of Code 2024, Day 9
-------------------------------------------
--  Disk Fragmenter
--
--  https://adventofcode.com/2024/day/9
--  Copy of questions in: aoc_2024_09_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

with Interfaces;

procedure AoC_2024_09 is

  use AoC_Toolbox, HAT, Interfaces;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2024_09";

  r : array (Part_Type) of Integer_64;
  s : VString;
  type Disk is array (0 .. 180_000) of Integer;
  d : Disk;
  l : Integer := -1;  --  Last block
  free : constant := -1;

  type Block_Descriptor is record
    id  : Natural;
    pos : Natural;
    len : Natural;  --  Possibly length of free blocks becomes 0.
  end record;

  --  Following arrays are "cheap vectors".
  --  In "full Ada" we would use Ada.Containers.Vectors
  --
  free_list, file_list : array (1 .. 10_000) of Block_Descriptor;
  last_free, last_file : Natural := 0;

  procedure Read_Data is
    f_in : File_Type;
    b : Natural;
    is_file : Boolean := True;
    invalid_file_id : constant := -1;
    file_id : Integer := invalid_file_id;
  begin
    Open (f_in, input_name & ".txt");
    Get_Line (f_in, s);
    Close (f_in);

    for i in 1 .. Length (s) loop
      b := Character'Pos (Element (s, i)) - Character'Pos ('0');
      if is_file then
        file_id := file_id + 1;
        --  Files are always of length > 0
        last_file := last_file + 1;
        file_list (last_file).id  := file_id;
        file_list (last_file).pos := l + 1;
        file_list (last_file).len := b;
        for j in 1 .. b loop
          l := l + 1;
          d (l) := file_id;
        end loop;
      else
        if b > 0 then
          last_free := last_free + 1;
          free_list (last_free).id  := 0;  --  Don't care
          free_list (last_free).pos := l + 1;
          free_list (last_free).len := b;
        end if;
        for j in 1 .. b loop
          l := l + 1;
          d (l) := free;
        end loop;
      end if;
      is_file := not is_file;
    end loop;
  end Read_Data;

  procedure Do_Part_1 is
    k : Integer;
    dc : Disk := d;
    lc : Integer := l;
  begin
    --  Move blocks to compact the disk:
    k := 0;
    while k < lc loop
      if dc (k) = free then
        while lc > k and then dc (lc) = free loop
          lc := lc - 1;
        end loop;
        exit when k >= lc;
        --  dc(lc) is a file block to be moved.
        dc (k) := dc (lc);
        lc := lc - 1;
      end if;
      k := k + 1;
    end loop;

    --  Checksum:
    for i in 0 .. lc loop
      r (part_1) := r (part_1) + Integer_64 (i * dc (i));
    end loop;
  end Do_Part_1;

  procedure Do_Part_2 is
    dc : Disk := d;
    from, to : Integer;
  begin
    --  Move blocks to compact the disk:
    for i in reverse 1 .. last_file loop
      for j in 1 .. last_free loop
        exit when free_list (j).pos > file_list (i).pos;
        --  Free space candidate is left to the file first block's position.
        if free_list (j).len >= file_list (i).len then
          --  There is enough room on the left to move the entire file
          --  as contiguous blocks.
          --  NB: AoC is kind and did not require overlapping moves (would
          --  have been a slightly more difficult special case)...
          from := file_list (i).pos;
          to   := free_list (j).pos;
          for k in reverse 0 .. file_list (i).len - 1 loop
            dc (to + k) := dc (from + k);
            dc (from + k) := free;
          end loop;
          --  Update file & free lists:
          file_list (i).pos := free_list (j).pos;
          free_list (j).pos := free_list (j).pos + file_list (i).len;
          free_list (j).len := free_list (j).len - file_list (i).len;
          exit;
        end if;
      end loop;
    end loop;

    --  Checksum
    for i in 0 .. l loop
      if dc (i) /= free then
        r (part_2) := r (part_2) + Integer_64 (i * dc (i));
      end if;
    end loop;
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

begin
  r (part_1) := 0;
  r (part_2) := 0;
  Read_Data;
  Do_Part_1;
  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1))) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Do_Part_2;
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put (+"Part 1:"); Put (r (part_1)'Image); New_Line;
    Put (+"Part 2:"); Put (r (part_2)'Image); New_Line;
    --  Part 1: validated by AoC: 6340197768906
    --  Part 2: validated by AoC: 6363913128533
  end if;
end AoC_2024_09;
