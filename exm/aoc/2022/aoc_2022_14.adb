--  Solution to Advent of Code 2022, Day 14
-------------------------------------------
--  Regolith Reservoir
--
--  https://adventofcode.com/2022/day/14
--  Copy of questions in: aoc_2022_14_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_14 is
  use HAT;

  subtype Range_x is Integer range 300 .. 800;
  subtype Range_y is Integer range   0 .. 200;

  map : array (Range_x, Range_y) of Character;

  type Point is record
    x, y : Integer;
  end record;

  m, n : Point;

  function Dist_L1 (a, b : Point) return Natural is
  begin
    return
      abs (a.x - b.x) + abs (a.y - b.y);
  end Dist_L1;

  procedure Adapt_Top_Left_Corner (using : Point) is
  begin
    m.x := Min (m.x, using.x);
    m.y := Min (m.y, using.y);
  end Adapt_Top_Left_Corner;

  procedure Adapt_Bottom_Right_Corner (using : Point) is
  begin
    n.x := Max (n.x, using.x);
    n.y := Max (n.y, using.y);
  end Adapt_Bottom_Right_Corner;

  procedure Show (title : VString) is
  begin
    New_Line;
    Put_Line (title);
    New_Line;
    Put_Line (+"  x is in range: " & m.x & " .. " & n.x);
    Put_Line (+"  y is in range: " & m.y & " .. " & n.y);
    New_Line;
    for y in m.y .. n.y loop
      Put ("  ");
      for x in m.x .. n.x loop
        Put (map (x, y));
      end loop;
      New_Line;
    end loop;
  end Show;

  procedure Data_Acquisition (enhanced : Boolean) is
    p1, p2, p : Point;
    arrow : String (1 .. 4) := " -> ";
    sep : Character;
    f : File_Type;
  begin
    m.x := Range_x'Last;
    m.y := 0;
    n.x := Range_x'First;
    n.y := Range_y'First;
    for x in Range_x loop
      for y in Range_y loop
        map (x, y) := '.';
      end loop;
    end loop;
    Open (f, "aoc_2022_14.txt");
  Read_Data :
    while not End_Of_File (f) loop
      Get (f, p1.x);
      Get (f, sep);
      Get (f, p1.y);
      Adapt_Top_Left_Corner (p1);
      Adapt_Bottom_Right_Corner (p1);
      map (p1.x, p1.y) := '#';
      loop
        Get (f, arrow);
        Get (f, p2.x);
        Get (f, sep);
        Get (f, p2.y);
        Adapt_Top_Left_Corner (p2);
        Adapt_Bottom_Right_Corner (p2);
        p := p1;
        for count in 1 .. Dist_L1 (p1, p2) loop
          p.x := p.x + Sgn (p2.x - p1.x);
          p.y := p.y + Sgn (p2.y - p1.y);
          map (p.x, p.y) := '#';
        end loop;
        exit when End_Of_Line (f);
        p1 := p2;
      end loop;
    end loop Read_Data;
    Close (f);
    if enhanced then
      --  Added a layer of rock 2 units below:
      p.y := n.y + 2;
      p.x := m.x - p.y;
      Adapt_Top_Left_Corner (p);
      p.x := n.x + p.y;
      Adapt_Bottom_Right_Corner (p);
      for x in m.x .. n.x loop
        map (x, p.y) := '@';
      end loop;
    end if;
  end Data_Acquisition;

  procedure Simulate_Sand_Unit (overflow : out Boolean) is
    p : Point;
  begin
    p.x := 500;
    p.y := Range_y'First;
    overflow := False;
    if map (p.x, p.y) = 'o' then
      --  Source is blocked!
      overflow := True;
      return;
    end if;
    loop
      if map (p.x, p.y + 1) = '.' then
        --  Fall further.
        p.y := p.y + 1;
      else
        --  Obstacle
        if map (p.x - 1, p.y + 1) = '.' then
          --  Move diagonally (left).
          p.x := p.x - 1;
          p.y := p.y + 1;
        elsif map (p.x + 1, p.y + 1) = '.' then
          --  Move diagonally (right).
          p.x := p.x + 1;
          p.y := p.y + 1;
        else
          map (p.x, p.y) := 'o';
          Adapt_Top_Left_Corner (p);  --  Displayed portion of the map may increase.
          exit;
        end if;
      end if;
      if p.y = n.y then
        --  Reach floor level: will "fall into the endless void".
        overflow := True;
        exit;
      end if;
    end loop;
  end Simulate_Sand_Unit;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  verbose : constant Boolean := False;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

  overflow : Boolean;

begin
Parts :
  for part in 1 .. 2 loop
    Data_Acquisition (part = 2);
    if verbose then
      Show (+"Initial:");
    end if;
    for step in Positive loop
      Simulate_Sand_Unit (overflow);
      if verbose and then n.y < 100 then
        Show (+"Sand unit #" & step & ':');
      end if;
      if overflow then
        r (part) := step - 1;
        --  ^ "- 1" is because "How many units of sand come to rest
        --    *before* sand starts flowing into the abyss below"
        exit;
      end if;
    end loop;
    if verbose then
      Show (+"Final:");
    end if;
    exit when compiler_test_mode;  --  Skip part 2.
  end loop Parts;

  if compiler_test_mode then
    if r (1) /= Integer'Value (To_String (Argument (1))) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Number of sand units for...");
    Put_Line (+"  (part 1): original map: " & r (1));
    Put_Line (+"  (part 2): enhanced map: " & r (2));
    --  Part 1: validated by AoC: 964
    --  Part 2: validated by AoC: 32041
  end if;
end AoC_2022_14;
