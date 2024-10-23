--  https://rosettacode.org/wiki/Determine_if_two_triangles_overlap
--  This version is adapted to HAC's Ada subset.

with HAT;

procedure Triangles_Overlap is

  use HAT;

  subtype Vertex is Natural range 0 .. 2;
  --  ^ Full Ada: type Vertex is mod 3;  (we simulate that with a range and explicit "mod 3"s)
  type Point is array (1 .. 2) of Real;
  type Triangle is array (Vertex) of Point;

  function Same_Side (A, B, M, N : Point) return Boolean is
  
    function Z_of_Cross_Product_AB_AU (U : Point) return Real is
    begin
      return
        (B (2) - A (2)) * (U (1) - A (1)) -
        (B (1) - A (1)) * (U (2) - A (2));
    end Z_of_Cross_Product_AB_AU;
    
  begin
    --  If the Z-value of AB^AM and AB^AN have the same sign, or one or both is 0,
    --  then the segment MN doesn't intersect the line containing segment AB.
    return Z_of_Cross_Product_AB_AU (M) * Z_of_Cross_Product_AB_AU (N) >= 0.0;
  end Same_Side;

  function In_Side (t1, t2 : Triangle) return Boolean is
    cond_1, cond_2 : Boolean;
  begin
    --  Simulate Ada 2012's "for all v in Vertex =>":
    cond_1 := True;
    for v1 in Vertex loop
      --  Simulate Ada 2012's "for some v2 in Vertex =>":
      cond_2 := False;
      for v2 in Vertex loop
        cond_2 := cond_2 or Same_Side (t1 ((v1 + 1) mod 3), t1 ((v1 + 2) mod 3), t1 (v1), t2 (v2));
      end loop;
      cond_1 := cond_1 and cond_2;
    end loop;
    return cond_1;
  end In_Side;

  function Overlap (t1, t2 : Triangle) return Boolean is
  begin
    return In_Side (t1, t2) and then In_Side (t2, t1);
  end Overlap;

  procedure To_Triangle (r1, r2, r3, r4, r5, r6 : Real; t : out Triangle) is
  begin
    t (0)(1) := r1;
    t (0)(2) := r2;
    t (1)(1) := r3;
    t (1)(2) := r4;
    t (2)(1) := r5;
    t (2)(2) := r6;
  end To_Triangle;

  procedure Show (T1, T2 : Triangle) is
  begin
    Put_Line (Boolean'Image (Overlap (T1, T2)));  --  !!  Overlap (T1, T2)'Image not accepted by HAC ??
  end Show;

  t1, t2 : Triangle;

begin
  To_Triangle (0.0, 0.0, 5.0, 0.0, 0.0, 5.0, t1); To_Triangle   (0.0, 0.0,  5.0,  0.0,  0.0, 6.0, t2); Show (t1, t2);
  To_Triangle (0.0, 0.0, 0.0, 5.0, 5.0, 0.0, t1); To_Triangle   (0.0, 0.0,  0.0,  5.0,  5.0, 0.0, t2); Show (t1, t2);
  To_Triangle (0.0, 0.0, 5.0, 0.0, 0.0, 5.0, t1); To_Triangle (-10.0, 0.0, -5.0,  0.0, -1.0, 6.0, t2); Show (t1, t2);
  To_Triangle (0.0, 0.0, 5.0, 0.0, 2.5, 5.0, t1); To_Triangle   (0.0, 4.0,  2.5, -1.0,  5.0, 4.0, t2); Show (t1, t2);
  To_Triangle (0.0, 0.0, 1.0, 1.0, 0.0, 2.0, t1); To_Triangle   (2.0, 1.0,  3.0,  0.0,  3.0, 2.0, t2); Show (t1, t2);
  To_Triangle (0.0, 0.0, 1.0, 1.0, 0.0, 2.0, t1); To_Triangle   (2.0, 1.0,  3.0, -2.0,  3.0, 4.0, t2); Show (t1, t2);
  To_Triangle (0.0, 0.0, 1.0, 0.0, 0.0, 1.0, t1); To_Triangle   (1.0, 0.0,  2.0,  0.0,  1.0, 1.0, t2); Show (t1, t2);
end Triangles_Overlap;
