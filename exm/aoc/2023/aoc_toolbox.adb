package body AoC_Toolbox is

  procedure GCD_and_Bezout (a, b : in Integer; s, t, the_gcd : out Integer) is
    --  Finds the GCD and s, t for the
    --  ` GCD (a, b) = a * s + b * t ` factorization (Bezout theorem).
    --  Program 1.8, Introduction to number theory, RBJT Allenby & EJ Redfern
    ta, tb : array (1 .. 3) of Integer;
    q, r : Integer;
  begin
    ta (1) := 1;         tb (1) := 0;
    ta (2) := 0;         tb (2) := 1;
    ta (3) := a;         tb (3) := b;
    while tb (3) /= 0 loop
      q := ta (3) / tb (3);
      for i in 1 .. 3 loop
        r := ta (i) - q * tb (i);
        ta (i) := tb (i);
        tb (i) := r;
      end loop;
    end loop;
    s :=       ta (1);
    t :=       ta (2);
    the_gcd := ta (3);
  end GCD_and_Bezout;

  function GCD (a, b : Integer) return Integer is
    s, t, the_gcd : Integer;
  begin
    GCD_and_Bezout (a, b, s, t, the_gcd);
    return the_gcd;
  end GCD;

  function LCM (a, b : Integer) return Integer is
  begin
    return abs (a * b) / GCD (a, b);
  end LCM;

  procedure GCD_and_Bezout_64 (a, b : in Integer_64; s, t, the_gcd : out Integer_64) is
    --  Finds the GCD and s, t for the
    --  ` GCD (a, b) = a * s + b * t ` factorization (Bezout theorem).
    --  Program 1.8, Introduction to number theory, RBJT Allenby & EJ Redfern
    ta, tb : array (1 .. 3) of Integer_64;
    q, r : Integer_64;
  begin
    ta (1) := 1;         tb (1) := 0;
    ta (2) := 0;         tb (2) := 1;
    ta (3) := a;         tb (3) := b;
    while tb (3) /= 0 loop
      q := ta (3) / tb (3);
      for i in 1 .. 3 loop
        r := ta (i) - q * tb (i);
        ta (i) := tb (i);
        tb (i) := r;
      end loop;
    end loop;
    s :=       ta (1);
    t :=       ta (2);
    the_gcd := ta (3);
  end GCD_and_Bezout_64;

  function GCD_64 (a, b : Integer_64) return Integer_64 is
    s, t, the_gcd : Integer_64;
  begin
    GCD_and_Bezout_64 (a, b, s, t, the_gcd);
    return the_gcd;
  end GCD_64;

  function LCM_64 (a, b : Integer_64) return Integer_64 is
  begin
    return abs (a * b) / GCD_64 (a, b);
  end LCM_64;

  function Dist_L1 (a, b : Point) return Natural is
  begin
    return
      abs (a.x - b.x) + abs (a.y - b.y);
  end Dist_L1;

  function Dist_Max (a, b : Point) return Natural is
  begin
    return
      HAT.Max (abs (a.x - b.x), abs (a.y - b.y));
  end Dist_Max;

  function Opposite (d : Direction) return Direction is
  begin
    case d is
      when north => return south;
      when south => return north;
      when east  => return west;
      when west  => return east;
    end case;
  end Opposite;

  function Dist_L1_3D (a, b : Point_3D) return Natural is
  begin
    return
      abs (a.x - b.x) + abs (a.y - b.y) + abs (a.z - b.z);
  end Dist_L1_3D;

  function Dist_L1_3D_R (a, b : Point_3D_R) return HAT.Real is
    use HAT;
  begin
    return
      abs (a.x - b.x) + abs (a.y - b.y) + abs (a.z - b.z);
  end Dist_L1_3D_R;

  procedure Skip_till_Space (fff : in out HAT.File_Type; times : Positive) is
    c : Character;
    use HAT;
  begin
    for repeat in 1 .. times loop
      while not End_Of_File (fff) loop
        Get (fff, c);
        exit when c = ' ';
      end loop;
      exit when End_Of_File (fff);
    end loop;
  end Skip_till_Space;

  function Sgn_64 (iii : Interfaces.Integer_64) return Interfaces.Integer_64 is
  begin
    if iii > 0 then
      return 1;
    elsif iii < 0 then
      return -1;
    else
      return 0;
    end if;
  end Sgn_64;

  procedure Rotate (x, y : in out HAT.Real; a : HAT.Real) is
    use HAT;
    nx : Real;
  begin
    nx := Cos (a) * x - Sin (a) * y;
    y  := Sin (a) * x + Cos (a) * y;
    x  := nx;
  end Rotate;

  function Deg_2_Rad (a : HAT.Real) return HAT.Real is
    use HAT;
  begin
    return (Pi / 180.0) * a;
  end Deg_2_Rad;

  package body Hash_Maps is

    --  Hash map code taken from AoC_2023_15's implementation.
    --  HASH = Holiday ASCII String Helper :-).

    function HASH (s : HAT.VString) return Natural is
      h : Natural := 0;
      use HAT;
    begin
      for i in 1 .. Length (s) loop
        h := ((h + Ord (Element (s, i))) * 17) rem 256;
      end loop;
      return h;
    end HASH;

    procedure Clear (hm : out Hash_Map_Type) is
    begin
      for i in hm'Range loop
        hm (i).slots := 0;
      end loop;
    end Clear;

    procedure Insert
      (hm        : in out Hash_Map_Type;
       key       : in     HAT.VString;
       new_value : in     Integer;
       value     :    out Integer)
    is
      b : constant Natural := HASH (key);
      found : Boolean;
      use HAT;
    begin
      found := False;
      for s in 1 .. hm (b).slots loop
        if hm (b).slot (s).key = key then
          found := True;
          value := hm (b).slot (s).value;
          exit;
        end if;
      end loop;
      if not found then
        --  Append new value.
        hm (b).slots := hm (b).slots + 1;
        hm (b).slot (hm (b).slots).key   := key;
        hm (b).slot (hm (b).slots).value := new_value;
        value := new_value;
      end if;
    end Insert;

    procedure Find
      (hm              : in out Hash_Map_Type;
       key             : in     HAT.VString;
       not_found_value : in     Integer;
       value           :    out Integer)
    is
      b : constant Natural := HASH (key);
      found : Boolean;
      use HAT;
    begin
      found := False;
      for s in 1 .. hm (b).slots loop
        if hm (b).slot (s).key = key then
          found := True;
          value := hm (b).slot (s).value;
          return;
        end if;
      end loop;
      if not found then
        value := not_found_value;
      end if;
    end Find;

  end Hash_Maps;

end AoC_Toolbox;
