--  Solution to Advent of Code 2023, Day $
------------------------------------------
--  .
--
--  https://adventofcode.com/2023/day/$
--  Copy of questions in: aoc_2023_$_questions.txt
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

with Interfaces;

procedure AoC_2023_24 is

  use AoC_Toolbox, HAT, Interfaces;

  capacity : constant := 1500;

  type Hail_Stone is record
    pos, vel : Point_3D_R;
  end record;

  type Hail_Stone_Array is array (1 .. capacity) of Hail_Stone;

  type List_Type is record
    stone           : Hail_Stone_Array;
    last            : Natural;
  end record;

  --  input_name : VString :=  +"mini_24"; skip_header : Natural := 0;
  --  min_box : Real := 7.0; max_box : Real := 27.0;
  --
  input_name : constant VString :=  +"aoc_2023_24";
  min_box : constant Real := 200000000000000.0; max_box : constant Real := 400000000000000.0;

  procedure Read_Data (list : out List_Type) is
    dummy : Character;
    f : File_Type;
    pos, vel : Point_3D_R;
  begin
    list.last := 0;
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      Get (f, pos.x); Get (f, dummy);
      Get (f, pos.y); Get (f, dummy);
      Get (f, pos.z);
      Get (f, dummy);
      Get (f, dummy);
      Get (f, vel.x); Get (f, dummy);
      Get (f, vel.y); Get (f, dummy);
      Get (f, vel.z);
      --
      list.last := list.last + 1;
      list.stone (list.last).pos := pos;
      list.stone (list.last).vel := vel;
    end loop;
    Close (f);
  end Read_Data;

  r : array (Part_Type) of Integer_64;

  data : List_Type;

  verbose : constant Boolean := False;
  research_mode : constant Boolean := False;

  procedure Intersect_Positive_Time
    (ia, ib : Integer; inter_x, inter_y : out Real; hit : out Boolean)
  is
    ax, ay, bx, by, ux, uy, vx, vy, alpha, beta, den : Real;
    show_intersection_times : constant Boolean := False;
    eps : constant := 0.0000001;
  begin
    --  We look for times alpha, beta such that
    --
    --      a + alpha * u = b + beta * v
    --
    --  The times alpha and beta don't need to be equal:
    --    "The hailstones themselves don't have to collide,
    --     just test for intersections between the paths they will trace."
    --
    hit := False;
    ax := data.stone (ia).pos.x;
    ay := data.stone (ia).pos.y;
    bx := data.stone (ib).pos.x;
    by := data.stone (ib).pos.y;
    ux := data.stone (ia).vel.x;
    uy := data.stone (ia).vel.y;
    vx := data.stone (ib).vel.x;
    vy := data.stone (ib).vel.y;
    --
    if abs ux < eps then
      if abs vx < eps then
        hit := abs (ax - bx) < eps and then abs (ay - by) < eps;
        beta := 0.0;
      else
        hit := True;
        beta := (ax - bx) / vx;
      end if;
    elsif abs uy < eps then
      if abs vy < eps then
        hit := abs (ax - bx) < eps and then abs (ay - by) < eps;
        beta := 0.0;
      else
        hit := True;
        beta := (ay - by) / vy;
      end if;
    else
      --  Both ux and uy are not 0
      den := (vx * uy - vy * ux);
      if abs den > eps then
        hit := True;
        beta := (ax * uy - ay * ux - bx * uy + by * ux) / den;
      end if;
    end if;
    if hit then
      hit := beta > 0.0;
    end if;
    if hit then
      --  Only positive time is considered: alpha and beta must be > 0.
      if abs ux > eps then
        alpha := (bx + vx * beta - ax) / ux;
        hit := alpha > 0.0;
      end if;
    end if;
    if hit then
      inter_x := bx + beta * vx;
      inter_y := by + beta * vy;
      hit := inter_x >= min_box and then inter_x <= max_box and then
             inter_y >= min_box and then inter_y <= max_box;
    end if;
    if hit then
      if verbose then
        Put_Line
          (+"Intersection within test area: " & ia & " with " & ib &
             " at point " & inter_x & ", " & inter_y &
             " beta = " & beta);
      end if;
      if research_mode and show_intersection_times then
        Put_Line
          (+"" &
           ia & ';' &
           ib & ';' &
           alpha & ';' &
           beta);
      end if;
    end if;
  end Intersect_Positive_Time;

  procedure Do_Part_1 is
    inter_x, inter_y : Real;
    hit : Boolean;
    show_intersection_times : constant Boolean := False;
  begin
    if research_mode and show_intersection_times then
      Put_Line (+"index a;index b;time alpha;time beta");
    end if;
    r (part_1) := 0;
    for ib in 1 .. data.last loop   --  Hail stone at b, velocity u.
      for ia in 1 .. ib - 1 loop    --  Hail stone at a, velocity v.
        Intersect_Positive_Time (ia, ib, inter_x, inter_y, hit);
        if hit then
          r (part_1) := r (part_1) + 1;
        end if;
      end loop;
    end loop;
    if research_mode and show_intersection_times then
      New_Line;
    end if;
  end Do_Part_1;

  procedure Research_Part_2 is
    dist_triplet : Real;
    x_common, y_common, z_common : Natural;
    low_velocities : constant Boolean := False;
    sel_1 : constant := 284; sel_2 : constant := 42; sel_3 : constant := 24;
  begin
    if low_velocities then
      Put_Line (+"index i;index j;index k;sum of L1 of velocities");
      --  We list point triplets with mutual velocities that are
      --  close to each other.
      for i in 1 .. data.last loop
        for j in 1 .. i - 1 loop
          for k in 1 .. j - 1 loop
            dist_triplet :=
              Dist_L1_3D_R (data.stone (i).vel, data.stone (j).vel) +
              Dist_L1_3D_R (data.stone (j).vel, data.stone (k).vel) +
              Dist_L1_3D_R (data.stone (k).vel, data.stone (i).vel);
            if dist_triplet < 75.0 then
              Put_Line (+"" & i & ';' & j & ';' & k & ';' & dist_triplet);
            end if;
          end loop;
        end loop;
      end loop;
      --
      New_Line;
      Put_Line
        (+"" & sel_1 & ';' &
         data.stone (sel_1).vel.x & ';' &
         data.stone (sel_1).vel.y & ';' &
         data.stone (sel_1).vel.z);
      Put_Line
        (+"" & sel_2 & ';' &
         data.stone (sel_2).vel.x & ';' &
         data.stone (sel_2).vel.y & ';' &
         data.stone (sel_2).vel.z);
      Put_Line
        (+"" & sel_3 & ';' &
         data.stone (sel_3).vel.x & ';' &
         data.stone (sel_3).vel.y & ';' &
         data.stone (sel_3).vel.z);
    end if;
    New_Line;
    Put_Line ("Same velocity component as some other");
    for ia in 1 .. data.last loop
      x_common := 0;
      y_common := 0;
      z_common := 0;
      for ib in 1 .. data.last loop
        if ia /= ib then
          if data.stone (ia).vel.x = data.stone (ib).vel.x then
            x_common := x_common + 1;
          end if;
          if data.stone (ia).vel.y = data.stone (ib).vel.y then
            y_common := y_common + 1;
          end if;
          if data.stone (ia).vel.z = data.stone (ib).vel.z then
            z_common := z_common + 1;
          end if;
        end if;
      end loop;
      if x_common > 0 or y_common > 0 or z_common > 0 then
        Put_Line
          (+"" & ia & ';' &
           x_common & ';' &
           y_common & ';' &
           z_common & ';');
      end if;
    end loop;
  end Research_Part_2;

  --  procedure Do_Part_2 is
  --    xb, yb, zb, v, w : Integer;
  --    ur, vr, wr, xb_r, inverse_tb : Real;
  --    eps : constant := 0.005;
  --  begin
  --    for ia in 1 .. data.last loop
  --      for ib in ia + 1 .. data.last loop
  --        if data.stone (ia).vel.x = data.stone (ib).vel.x then
  --          if verbose then
  --            Put_Line (+"" & ia & ", " & ib);
  --          end if;
  --          xb := data.stone (ib).pos.x - data.stone (ia).pos.x;
  --          if xb = 0 then
  --            Put ("!!");
  --          else
  --            yb := data.stone (ib).pos.y - data.stone (ia).pos.y;
  --            zb := data.stone (ib).pos.z - data.stone (ia).pos.z;
  --            for u in -1000 .. 1000 loop
  --              if u /= 0 then
  --                ur := Real (u);  --  Test relative velocity u on x axis
  --                  xb_r := Real (xb);
  --                  inverse_tb := ur / xb_r;
  --                  vr := Real (yb) * inverse_tb;
  --                  wr := Real (zb) * inverse_tb;
  --                  if          abs (vr - Real (Trunc (vr))) < eps
  --                     and then abs (wr - Real (Trunc (wr))) < eps then
  --                    --  vr and wr are actually integers.
  --                    Put_Line
  --                      (+"Ah ha! " & ia & ", " & ib &
  --                        ", u = " & ur &
  --                        ", v = " & vr &
  --                        ", w = " & wr);
  --                    v := Integer (vr);
  --                    w := Integer (wr);
  --                    --  Absolute values
  --                  end if;
  --                end if;
  --            end loop;
  --          end if;
  --        end if;
  --      end loop;
  --    end loop;
  --  end Do_Part_2;

  --  The following is an adaptation to the HAC Ada subset of the 2nd solution
  --  to 2nd part by John Perry:
  --
  --      https://github.com/johnperry-math/AoC2023/tree/master/day24

  procedure Do_Part_2_By_GB is

    First, Second, Third : Hail_Stone;
    --  three non-parallel stones

    Found : Boolean := False;
    --  whether we've found the stones yet

    type Reduction_Matrix is array (1 .. 6, 1 .. 7) of Real;
    Matrix  : Reduction_Matrix;
    Pivoter : Real;

    X, Y, Z : Real;
    --  the solution

    inter_x, inter_y : Real;
    valid : Boolean;

    eps : constant := 0.0000001;

  begin

    --  find 3 intersecting stones

  Outer_Loop :
    for Ith in data.stone'First .. data.last - 2 loop
      for Jth in Ith + 1 .. data.last - 1 loop
        Intersect_Positive_Time (Ith, Jth, inter_x, inter_y, valid);
        if valid then
          First  := data.stone (Ith);
          Second := data.stone (Jth);
          for Kth in Jth + 1 .. data.last loop
            Intersect_Positive_Time (Ith, Jth, inter_x, inter_y, valid);
            if valid then
              Third := data.stone (Kth);
              Found := True;
              exit Outer_Loop;
            end if;
          end loop;
        end if;
      end loop;
    end loop Outer_Loop;

    if not Found then
      Put_Line
        ("Mayday! Did not find three stones" &
         " with mutual intersection in the future!");
    end if;

    --  matrix columns set up as
    --  | dx | dy | dz | x | y | z |
    --  we want only x, y, z, so this makes for a little less work
    --
    --  matrix coefficients determined by some Groebner basis-like methods:
    --
    --  * we want to know x, y, z, dx, dy, dz
    --  * we know First, Second, Third
    --  * the problem implies the following equations:
    --       x + dx * t1 = First.pos.x + First.vel.x * t1
    --    ...and so forth for y, z, dy, dz, t2, t3, Second, Third
    --  * rewrite the equations to eliminate the quadratic terms
    --    and you get equations with the coefficients below
    Matrix (1, 1) := First.pos.y - Second.pos.y;
    Matrix (1, 2) := Second.pos.x - First.pos.x;
    Matrix (1, 3) := 0.0;
    Matrix (1, 4) := Second.vel.y - First.vel.y;
    Matrix (1, 5) := First.vel.x - Second.vel.x;
    Matrix (1, 6) := 0.0;
    Matrix (1, 7) := -(First.pos.x * First.vel.y - First.pos.y * First.vel.x
                       - Second.pos.x * Second.vel.y + Second.pos.y * Second.vel.x);
    Matrix (2, 1) := First.pos.z - Second.pos.z;
    Matrix (2, 2) := 0.0;
    Matrix (2, 3) := Second.pos.x - First.pos.x;
    Matrix (2, 4) := Second.vel.z - First.vel.z;
    Matrix (2, 5) := 0.0;
    Matrix (2, 6) := First.vel.x - Second.vel.x;
    Matrix (2, 7) := -(First.pos.x * First.vel.z - First.pos.z * First.vel.x
                       - Second.pos.x * Second.vel.z + Second.pos.z * Second.vel.x);
    Matrix (3, 1) := 0.0;
    Matrix (3, 2) := First.pos.z - Second.pos.z;
    Matrix (3, 3) := Second.pos.y - First.pos.y;
    Matrix (3, 4) := 0.0;
    Matrix (3, 5) := Second.vel.z - First.vel.z;
    Matrix (3, 6) := First.vel.y - Second.vel.y;
    Matrix (3, 7) := -(First.pos.y * First.vel.z - First.pos.z * First.vel.y
                       - Second.pos.y * Second.vel.z + Second.pos.z * Second.vel.y);
    Matrix (4, 1) := First.pos.y - Third.pos.y;
    Matrix (4, 2) := Third.pos.x - First.pos.x;
    Matrix (4, 3) := 0.0;
    Matrix (4, 4) := Third.vel.y - First.vel.y;
    Matrix (4, 5) := First.vel.x - Third.vel.x;
    Matrix (4, 6) := 0.0;
    Matrix (4, 7) := -(First.pos.x * First.vel.y - First.pos.y * First.vel.x
                       - Third.pos.x * Third.vel.y + Third.pos.y * Third.vel.x);
    Matrix (5, 1) := First.pos.z - Third.pos.z;
    Matrix (5, 2) := 0.0;
    Matrix (5, 3) := Third.pos.x - First.pos.x;
    Matrix (5, 4) := Third.vel.z - First.vel.z;
    Matrix (5, 5) := 0.0;
    Matrix (5, 6) := First.vel.x - Third.vel.x;
    Matrix (5, 7) := -(First.pos.x * First.vel.z - First.pos.z * First.vel.x
                       - Third.pos.x * Third.vel.z + Third.pos.z * Third.vel.x);
    Matrix (6, 1) := 0.0;
    Matrix (6, 2) := First.pos.z - Third.pos.z;
    Matrix (6, 3) := Third.pos.y - First.pos.y;
    Matrix (6, 4) := 0.0;
    Matrix (6, 5) := Third.vel.z - First.vel.z;
    Matrix (6, 6) := First.vel.y - Third.vel.y;
    Matrix (6, 7) := -(First.pos.y * First.vel.z - First.pos.z * First.vel.y
                       - Third.pos.y * Third.vel.z + Third.pos.z * Third.vel.y);

    --  make upper-triangular
    --  the following approach is quite naive and not very good in general
    for Pivot in 1 .. 5 loop

      if abs Matrix (Pivot, Pivot) < eps then
        for Col in Pivot .. 7 loop
          Pivoter                 := Matrix (Pivot, Col);
          Matrix (Pivot, Col)     := Matrix (Pivot + 1, Col);
          Matrix (Pivot + 1, Col) := Pivoter;
        end loop;
      end if;

      for Row in Pivot + 1 .. 6 loop
        Pivoter := Matrix (Row, Pivot);
        for Col in Pivot .. 7 loop
          Matrix (Row, Col) :=
            Matrix (Row, Col) - Pivoter / Matrix (Pivot, Pivot) * Matrix (Pivot, Col);
        end loop;
      end loop;

    end loop;

    --  now clear last two non-pivot columns in last 3 rows
    for Row in 4 .. 5 loop
      Pivoter := Matrix (Row, 6);
      for Col in 6 .. 7 loop
        Matrix (Row, Col) := Matrix (Row, Col) - Pivoter / Matrix (6, 6) * Matrix (6, Col);
      end loop;
    end loop;
    Pivoter := Matrix (4, 5);
    for Col in 5 .. 7 loop
      Matrix (4, Col) := Matrix (4, Col) - Pivoter / Matrix (5, 5) * Matrix (5, Col);
    end loop;

    --  we now have a matrix whose last 3 rows have the form
    --    0 0 0 a 0 0 b
    --    0 0 0 0 c 0 d
    --    0 0 0 0 0 e f
    --  this is easy to solve

    X := Matrix (4, 7) / Matrix (4, 4);
    Y := Matrix (5, 7) / Matrix (5, 5);
    Z := Matrix (6, 7) / Matrix (6, 6);

    r (part_2) := Integer_64 (X + Y + Z);

  end Do_Part_2_By_GB;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;

begin
  Read_Data (data);

  Do_Part_1;

  if research_mode then
    Research_Part_2;
  end if;

  Do_Part_2_By_GB;

  if compiler_test_mode then
    if r (part_1) /= Integer_64'Value (To_String (Argument (1))) or
       r (part_2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1)'Image);
    Put_Line (+"Part 2: " & r (part_2)'Image);
    --  Part 1: validated by AoC: 24192
    --  Part 2: validated by AoC: 664822352550558
  end if;
end AoC_2023_24;
