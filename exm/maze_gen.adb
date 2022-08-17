--  Special version (for HAC) of
--  http://rosettacode.org/wiki/Maze_generation#Ada

with HAT;

procedure Maze_Gen is

  type Direction is (North, South, West, East);

  type Cell_Walls is array (Direction) of Boolean;

  type Cells is record
    Walls   : Cell_Walls;
    Visited : Boolean;
  end record;

  Height : constant := 15;
  Width  : constant := 24;

  type Maze_Grid is array (1 .. Height, 1 .. Width) of Cells;

  type Point is record
    Row, Col : Integer;
  end record;

  type Maze_Type is record
    Start : Point;
    Grid  : Maze_Grid;
  end record;

  function Opposite (D : Direction) return Direction is
  begin
    case D is
      when North => return South;
      when South => return North;
      when East  => return West;
      when West  => return East;
    end case;
  end Opposite;

  procedure Move (P : in out Point; D : Direction; Valid_Move : out Boolean) is
  begin
    Valid_Move := False;
    case D is
      when North =>
        if P.Row > 1 then
          Valid_Move := True;
          P.Row      := P.Row - 1;
        end if;
      when East =>
        if P.Col < Width then
          Valid_Move := True;
          P.Col      := P.Col + 1;
        end if;
      when West =>
        if P.Col > 1 then
          Valid_Move := True;
          P.Col      := P.Col - 1;
        end if;
      when South =>
        if P.Row < Height then
          Valid_Move := True;
          P.Row      := P.Row + 1;
        end if;
    end case;
  end Move;

  function Val (I : Integer) return Direction is  --  ! Full Ada: use 'Val(...)
  begin
    case I is
      when 0      => return North;
      when 1      => return South;
      when 2      => return West;
      when others => return East;
    end case;
  end Val;

  procedure Depth_First_Algorithm (Maze : in out Maze_Grid; P : Point) is
    --  https://en.wikipedia.org/wiki/Maze_generation_algorithm
    Next_P          : Point;
    Next_D          : Direction;
    Valid_Direction : Boolean;
    Checked_Wall    : array (Direction) of Boolean;
    All_Checked     : Boolean;
  begin
    for D in Direction loop  --  ! Full Ada ":= (others => False)"
      Checked_Wall (D) := False;
    end loop;
    --  Mark as visited:
    Maze (P.Row, P.Col).Visited := True;
    loop
      --  Use random direction:
      loop
        Next_D := Val (HAT.Rand (3));
        exit when not Checked_Wall (Next_D);  --  At least one wall is unchecked.
      end loop;
      Next_P := P;
      Move (Next_P, Next_D, Valid_Direction);
      if Valid_Direction then
        if not Maze (Next_P.Row, Next_P.Col).Visited then
          --  Connect the two cells by breaking the wall.
          Maze (P.Row, P.Col).Walls (Next_D) := False;
          Maze (Next_P.Row, Next_P.Col).Walls (Opposite (Next_D)) := False;
          --  Recurse from next cell.
          Depth_First_Algorithm (Maze, Next_P);
        end if;
      end if;
      Checked_Wall (Next_D) := True;  --  Neighbour is either visited or invalid.
      --
      All_Checked := True;
      for D in Direction loop
        All_Checked := All_Checked and Checked_Wall (D);
      end loop;
      --  Neighbours in all directions are either visited
      --  (either from here, or previously visited), or invalid:
      exit when All_Checked;
      --  Continue until there is no unvisited neighbour left.
    end loop;
  end Depth_First_Algorithm;

  procedure Initialize (Maze : in out Maze_Type) is
    All_Walls : Cell_Walls;
    use HAT;
  begin
    for D in Direction loop
      All_Walls (D) := True;
    end loop;
    --  ! Full Ada: initialized record.
    for i in 1 .. Height loop
      for j in 1 .. Width loop
        Maze.Grid (i, j).Walls := All_Walls;
        Maze.Grid (i, j).Visited := False;
      end loop;
    end loop;
    --  Choose starting cell
    Maze.Start.Row := Rand (1_000_000) mod Height + 1;
    Maze.Start.Col := Rand (1_000_000) mod Width + 1;
    Put_Line
       (+"Height: " & Height & ", Width: " & Width);
    Put_Line
       (+"Starting generation at " &
        Maze.Start.Row & ", " & Maze.Start.Col);
    Depth_First_Algorithm (Maze.Grid, Maze.Start);
  end Initialize;

  procedure Put_Grid (Item : Maze_Type) is
    use HAT;
    H_Bar : array (Boolean) of VString;
    V_Bar : array (Boolean) of Character;
    S_Cell : array (Boolean) of VString;
    Line : VString;
  begin
    H_Bar (False) := +"   +";
    H_Bar (True)  := +"---+";
    V_Bar (False) := ' ';
    V_Bar (True)  := '|';
    S_Cell (False) := +"   ";
    S_Cell (True)  := +" X ";
    Line := +"+";
    for Col in 1 .. Width loop
      Line := Line & H_Bar (Item.Grid (1, Col).Walls (North));
    end loop;
    Put_Line (Line);
    for Row in 1 .. Height loop
      Line := +"" & V_Bar (Item.Grid (Row, 1).Walls (West));
      for Col in 1 .. Width loop
        Line := Line & S_Cell ((Row = Item.Start.Row) and (Col = Item.Start.Col))
             --  ! Full Ada: () useless in "() and ()" (HAC has wrong priority)  !!
                     & V_Bar (Item.Grid (Row, Col).Walls (East));
      end loop;
      Put_Line (Line);
      Line := +"+";
      for Col in 1 .. Width loop
        Line := Line & H_Bar (Item.Grid (Row, Col).Walls (South));
      end loop;
      Put_Line (Line);
    end loop;
  end Put_Grid;

  My_Maze : Maze_Type;

begin
  Initialize (My_Maze);
  Put_Grid (My_Maze);
end Maze_Gen;
