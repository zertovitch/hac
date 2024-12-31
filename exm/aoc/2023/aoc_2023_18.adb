--  Solution to Advent of Code 2023, Day 18
-------------------------------------------
--  Lavaduct Lagoon
--
--  https://adventofcode.com/2023/day/18
--  Copy of questions in: aoc_2023_18_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  The files aoc_toolbox.ad* are located in the upper directory (..)
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

with Interfaces;

procedure AoC_2023_18 is

  use AoC_Toolbox, HAT, Interfaces;

  verbose : constant Boolean := False;

  --  input_name : constant VString := +"mini"; n : constant := 22; start_1 : constant := 2;
  input_name : constant VString := +"aoc_2023_18"; n : constant := 822; start_1 : constant := 250;
  --

  type State is (clean, path, inside, outside);

  map : array (1 .. n, 1 .. n) of State;

  procedure Clean_Map is
  begin
    for y in 1 .. n loop
      for x in 1 .. n loop
        map (x, y) := clean;
      end loop;
    end loop;
  end Clean_Map;

  procedure Read_Data_Part_1 is
    c : Character;
    i : Integer;
    f : File_Type;
    pt : Point;
  begin
    Clean_Map;
    pt.x := start_1;
    pt.y := start_1;
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get (f, c);
      Get (f, i);
      for count in 1 .. i loop
        map (pt.x, pt.y) := path;
        case c is
          when 'U' => pt.y := pt.y - 1;
          when 'D' => pt.y := pt.y + 1;
          when 'L' => pt.x := pt.x - 1;
          when 'R' => pt.x := pt.x + 1;
          when others => null;
        end case;
      end loop;
      Skip_Line (f);
    end loop;
    Close (f);
  end Read_Data_Part_1;

  type Vector_Values is array (1 .. n) of Integer;

  type Ordered_Vector is record
    top : Natural;
    val : Vector_Values;
  end record;

  procedure Insert (v : in out Ordered_Vector; value : Integer) is
    ins : Positive;
    found : Boolean := False;
  begin
    for i in 1 .. v.top loop
      if v.val (i) = value then
        found := True;
        exit;
      end if;
    end loop;
    if not found then
      ins := v.top + 1;
      for i in 1 .. v.top loop
        if v.val (i) > value then
          ins := i;
          exit;
        end if;
      end loop;
      for i in reverse ins .. v.top loop
        v.val (i + 1) := v.val (i);
      end loop;
      v.val (ins) := value;
      v.top := v.top + 1;
    end if;
  end Insert;

  procedure Find
    (v     : in out Ordered_Vector;
             --  ^ "out" is not needed, we just force
             --    a by-reference parameter passing on HAC.
     value :        Integer;
     index :    out Positive)
  is
  begin
    for i in 1 .. v.top loop
      if v.val (i) = value then
        index := i;
        return;
      end if;
    end loop;
    index := Integer'Last;
  end Find;

  trx, try : Ordered_Vector;

  procedure Read_Data_Part_2 is
    c : Character;
    sep : String (1 .. 2);
    dummy, i, j, x1, x2, y1, y2 : Integer;
    f : File_Type;
    s : VString;
    old_pt, new_pt : Point;
  begin
    Clean_Map;
    new_pt.x := 0;
    new_pt.y := 0;
    old_pt := new_pt;
    trx.top := 0;
    try.top := 0;
    --  Register start point (coordinates: 0,0).
    --  We need some detailed pixellisation around each point.
    for dxy in -1 .. +1 loop
      Insert (trx, dxy);
      Insert (try, dxy);
    end loop;
  Passes :
    for pass in 1 .. 2 loop
      Open (f, input_name & ".txt");
      while not End_Of_File (f) loop
        Get (f, c);
        Get (f, dummy);
        Get (f, sep);
        Get_Line (f, s);
        Delete (s, 8, 8);
        i := Integer_Value (+"16" & s & '#');  --  Read hexadecimal part.
        j := i / 16;
        case i rem 16 is
          when 0 => new_pt.x := new_pt.x + j;  --  Right
          when 1 => new_pt.y := new_pt.y + j;  --  Down
          when 2 => new_pt.x := new_pt.x - j;  --  Left
          when 3 => new_pt.y := new_pt.y - j;  --  Up
          when others => null;
        end case;
        if pass = 1 then
          --  Register a point (if it is a new point).
          for dxy in -1 .. +1 loop
            Insert (trx, new_pt.x + dxy);
            Insert (try, new_pt.y + dxy);
          end loop;
        else
          --  Draw a line in the index map
          if new_pt.y = old_pt.y then
            --  Horizontal
            Find (trx, old_pt.x, x1);
            Find (trx, new_pt.x, x2);
            Find (try, new_pt.y, y1);
            for x12 in Min (x1, x2) .. Max (x1, x2) loop
              map (x12, y1) := path;
            end loop;
          else
            --  Vertical
            Find (trx, new_pt.x, x1);
            Find (try, old_pt.y, y1);
            Find (try, new_pt.y, y2);
            for y12 in Min (y1, y2) .. Max (y1, y2) loop
              map (x1, y12) := path;
            end loop;
          end if;
          --
          old_pt := new_pt;
        end if;
      end loop;
      Close (f);
    end loop Passes;

    if verbose then
      Put_Line (+"TRX " & trx.top);
      for tx in 1 .. trx.top loop
        Put_Line (trx.val (tx));
      end loop;
      Put_Line (+"TRY " & try.top);
      for ty in 1 .. try.top loop
        Put_Line (try.val (ty));
      end loop;
    end if;
  end Read_Data_Part_2;

  procedure Show is
    trunc_n : constant Integer := Min (70, n);
  begin

    Put_Line ("Map: -------------------");
    for y in 1 .. trunc_n loop
      for x in 1 .. trunc_n loop
        case map (x, y) is
          when clean   => Put ('.');
          when path    => Put ('#');
          when outside => Put ('o');
          when inside  => Put ('i');
        end case;
      end loop;
      New_Line;
    end loop;
  end Show;

  r : array (Part_Type) of Integer_64;

  procedure Do_Part (p : Part_Type) is
    c : Integer_64 := 0;

    procedure Flood_Fill (x, y : Integer) is  --  Taken from aoc_2023_10.
    begin
      if x in 1 .. n
        and then y in 1 .. n
        and then map (x, y) = clean
      then
        map (x, y) := outside;
        c := c + 1;
        Flood_Fill (x - 1, y);
        Flood_Fill (x + 1, y);
        Flood_Fill (x, y - 1);
        Flood_Fill (x, y + 1);
      end if;
    end Flood_Fill;

    width, height : array (1 .. n) of Positive;
  begin
    if verbose then
      Put_Line ("Part " & p'Image);
      Show;
    end if;
    Flood_Fill (1, 1);
    if verbose then
      Show;
    end if;
    case p is
      when part_1 =>
        r (p) := Integer_64 (n * n - c);
      when part_2 =>
        --  We need to compute the "real" surfaces
        c := 0;
        for ix in 1 .. trx.top loop
          if ix = trx.top then
            --  Last "big pixel" has width 1 (a decision)
            width (ix) := 1;
          else
            width (ix) := trx.val (ix + 1) - trx.val (ix);
          end if;
        end loop;
        for iy in 1 .. try.top loop
          if iy = try.top then
            --  Last "big pixel" has height 1 (a decision)
            height (iy) := 1;
          else
            height (iy) := try.val (iy + 1) - try.val (iy);
          end if;
        end loop;
        for iy in 1 .. try.top loop
          for ix in 1 .. trx.top loop
            if map (ix, iy) = outside then
              c := c + Integer_64 (width (ix) * height (iy));
            end if;
          end loop;
        end loop;
        r (p) := Integer_64 (trx.val (trx.top) - trx.val (1) + 1) *
                 Integer_64 (try.val (try.top) - try.val (1) + 1)
                 -
                 c;
    end case;
  end Do_Part;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  Read_Data_Part_1;
  Do_Part (part_1);

  Read_Data_Part_2;
  Do_Part (part_2);

  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1))) or
       r (part_2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1:" & r (part_1)'Image);
    Put_Line (+"Part 2:" & r (part_2)'Image);
    --  Part 1: validated by AoC: 53300
    --  Part 2: validated by AoC: 64294334780659
  end if;
end AoC_2023_18;
