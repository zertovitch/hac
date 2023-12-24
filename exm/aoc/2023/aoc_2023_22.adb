--  Solution to Advent of Code 2023, Day 22
------------------------------------------
--  Sand Slabs
--
--  https://adventofcode.com/2023/day/22
--  Copy of questions in: aoc_2023_22_questions.txt
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_22 is

  use AoC_Toolbox, HAT;

  capacity : constant := 1500;

  --  Poor man's ordered map (inspired from Day 18).
  --  On a "full Ada" system like GNAT we would use
  --  efficient Ada.Container.* goodies...

  type Vector_Values is array (1 .. capacity) of Integer;

  type Ordered_Map is record
    last : Natural;
    key  : Vector_Values;  --  Ordered values
    elt  : Vector_Values;  --  Elements
  end record;

  procedure Insert (v : in out Ordered_Map; key, element : Integer) is
    ins : Positive;
    found : Boolean := False;
  begin
    for i in 1 .. v.last loop
      if v.key (i) = key then
        ins := i;
        found := True;
        exit;
      end if;
    end loop;
    if not found then
      ins := v.last + 1;
    end if;
    for i in 1 .. v.last loop
      if v.key (i) > key then
        ins := i;
        exit;
      end if;
    end loop;
    --  Make room for the new element
    for i in reverse ins .. v.last loop
      v.key (i + 1) := v.key (i);
      v.elt (i + 1) := v.elt (i);
    end loop;
    v.key (ins) := key;
    v.elt (ins) := element;
    v.last := v.last + 1;
  end Insert;

  procedure Find
    (v     : in out Ordered_Map;
             -- ^ "out" is not needed, we just force
             --   a by-reference parameter passing on HAC.
     key   : in     Integer;
     index :    out Integer;
     found :    out Boolean)
  is
  begin
    for i in 1 .. v.last loop
      if v.key (i) = key then
        index := i;
        found := True;
        return;
      end if;
    end loop;
    found := False;
  end Find;

  procedure Delete_Index
    (v     : in out Ordered_Map;
     index : in     Integer)
  is
  begin
    for i in index + 1 ..  v.last loop
      v.key (i - 1) := v.key (i);
      v.elt (i - 1) := v.elt (i);
    end loop;
    v.last := v.last - 1;
  end Delete_Index;

  --  There may be multiple elements for the same key.

  procedure Delete_Element (map : in out Ordered_Map; key, element : Integer) is
    idx_ord_delete : Integer;
    key_found : Boolean;
    elt_found : Boolean;
  begin
    Find (map, key, idx_ord_delete, key_found);
    if key_found then
      elt_found := False;
      for j in idx_ord_delete .. map.last loop
        if map.elt (j) = element then
          --  Found our element in the map.
          Delete_Index (map, j);
          elt_found := True;
          exit;
        end if;
      end loop;
      if not elt_found then
        Put ("Element not found");
      end if;
    else
      Put ("Key not found");
    end if;
  end Delete_Element;

  type Brick is record
    edge_min, edge_max : Point_3D;
  end record;

  type Brick_Array is array (1 .. capacity) of Brick;

  type List_Type is record
    brick           : Brick_Array;
    last            : Natural;
    z_top, z_bottom : Ordered_Map;  --  Ordered tops and bottoms of bricks
  end record;

  --  GNAT runs this program on the actual problem data in 2.24 seconds,
  --  while HAC takes forever.

  compiler_test_mode : constant Boolean := Argument_Count >= 2;

  function Input_Name return VString is
  begin
    if compiler_test_mode then
      --  We grab the small data directly from the text!
      return +"aoc_2023_22_questions";
    else
      return +"aoc_2023_22";
    end if;
  end Input_Name;

  example_length : constant := 7;

  procedure Read_Data (list : out List_Type) is
    dummy : Character;
    f : File_Type;
    e_min, e_max : Point_3D;
  begin
    list.last := 0;
    list.z_top.last    := 0;
    list.z_bottom.last := 0;
    Open (f, Input_Name & ".txt");
    if compiler_test_mode then
      --  Skip x lines of bla-bla.
      Skip_Line (f, 13);
    end if;
    while not End_Of_File (f) loop
      Get (f, e_min.x); Get (f, dummy);
      Get (f, e_min.y); Get (f, dummy);
      Get (f, e_min.z);
      Get (f, dummy);
      Get (f, e_max.x); Get (f, dummy);
      Get (f, e_max.y); Get (f, dummy);
      Get (f, e_max.z);
      --
      list.last := list.last + 1;
      list.brick (list.last).edge_min := e_min;
      list.brick (list.last).edge_max := e_max;
      Insert (list.z_bottom, e_min.z, list.last);
      Insert (list.z_top,    e_max.z, list.last);
      --
      exit when compiler_test_mode and then list.last = example_length;
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer;

  procedure Fall
    (list     : in out List_Type;
     ignore   : in     Natural;
     simulate : in     Boolean;
     moves    :    out Natural)
  is
    own_z_bottom, own_z_top : Integer;
    idx_own, idx_ord_other, idx_other : Integer;
    found : Boolean;
    x1, x2, y1, y2 : Integer;
    xt1, xt2, yt1, yt2 : Integer;
    brick_can_move : Boolean;
  begin
    moves := 0;
  Check_Bottoms :
    for i in 1 .. list.z_bottom.last loop
      own_z_bottom := list.z_bottom.key (i);
      idx_own      := list.z_bottom.elt (i);
      own_z_top    := list.brick (idx_own).edge_max.z;
      brick_can_move := False;
    Iterate_Bottom :
      while own_z_bottom > 1 loop
        own_z_bottom := own_z_bottom - 1;
        own_z_top    := own_z_top    - 1;
        --  We let the bottom fall until it hits the top
        --  of another brick.
        Find (list.z_top, own_z_bottom, idx_ord_other, found);
        if found then
          x1 := list.brick (idx_own).edge_min.x;
          x2 := list.brick (idx_own).edge_max.x;
          y1 := list.brick (idx_own).edge_min.y;
          y2 := list.brick (idx_own).edge_max.y;
          while
            idx_ord_other <= list.z_top.last
            and then list.z_top.key (idx_ord_other) = own_z_bottom
          loop
            --  We have a series of "other" bricks whose top face
            --  might collide with "our" falling brick's bottom face.
            --  "Our" brick's bottom is at height `own_z_bottom`
            --  The "other" brick's top face is at the same height.
            --  If the rectangles intersect, the fall has clearly
            --  gone too far...
            idx_other := list.z_top.elt (idx_ord_other);
            if idx_other /= idx_own and then idx_other /= ignore then
              xt1 := list.brick (idx_other).edge_min.x;
              xt2 := list.brick (idx_other).edge_max.x;
              yt1 := list.brick (idx_other).edge_min.y;
              yt2 := list.brick (idx_other).edge_max.y;
              exit Iterate_Bottom when
                not
                  (xt2 < x1 or else  --  the "other" brick is left to our brick
                   xt1 > x2 or else  --  the "other" brick is right to our brick
                   yt2 < y1 or else  --  same for y axis
                   yt1 > y2);        --  same for y axis
            end if;
            idx_ord_other := idx_ord_other + 1;
          end loop;
        end if;
        brick_can_move := True;
        if simulate then
          moves := 1;  --  Just some non-zero value.
          exit Check_Bottoms;
          --  "Don't actually disintegrate any bricks - just determine
          --   what would happen if, for each brick, only that
          --   brick were disintegrated."
        end if;
        --  All right, we can safely move the brick down by -1.
        list.brick (idx_own).edge_min.z := own_z_bottom;
        list.brick (idx_own).edge_max.z := own_z_top;
        --  Keep top / bottom lists up-to-date.
        Delete_Element (list.z_top,    own_z_top + 1,    idx_own);
        Delete_Element (list.z_bottom, own_z_bottom + 1, idx_own);
        Insert (list.z_top,    own_z_top,    idx_own);
        Insert (list.z_bottom, own_z_bottom, idx_own);
      end loop Iterate_Bottom;
      if brick_can_move then
        moves := moves + 1;
      end if;
    end loop Check_Bottoms;
  end Fall;

  data : List_Type;

  procedure Do_Part_1 is
    list : List_Type := data;
    moves : Natural;
  begin
    --  "You'll need to start by figuring out where [the falling bricks]
    --   will end up."
    Fall (list, 0, False, moves);

    r (part_1) := 0;
    for brick_index in 1 .. list.last loop
      Fall (list, brick_index, True, moves);
      if moves = 0 then
        --  Brick #brick_index is safe to disintegrate.
        r (part_1) := r (part_1) + 1;
      end if;
    end loop;
  end Do_Part_1;

  procedure Do_Part_2 is
    list, work : List_Type := data;
    moves : Natural;
  begin
    Fall (list, 0, False, moves);
    r (part_2) := 0;
    for brick_index in 1 .. list.last loop
      work := list;
      Fall (work, brick_index, False, moves);
      --  Put_Line
      --    (+"Disintegrating brick #" & brick_index &
      --      " would cause " & moves & " to fall");
      r (part_2) := r (part_2) + moves;
    end loop;
  end Do_Part_2;

  T0 : constant Time := Clock;

begin

  Read_Data (data);

  Do_Part_1;

  Do_Part_2;

  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 451    (example: 5)
    --  Part 2: validated by AoC: 66530  (example: 7)
  end if;
end AoC_2023_22;
