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

  procedure Rotate (x, y : in out HAT.Real; a : HAT.Real) is
    use HAT;
    nx : Real;
  begin
    nx := Cos (a) * x - Sin (a) * y;
    y  := Sin (a) * x + Cos (a) * y;
    x  := nx;
  end Rotate;

  function Opposite (d : Direction) return Direction is
  begin
    case d is
      when north => return south;
      when south => return north;
      when east  => return west;
      when west  => return east;
    end case;
  end Opposite;

  function Turn_Right (d : Direction) return Direction is
  begin
    case d is
      when north => return east;
      when south => return west;
      when east  => return south;
      when west  => return north;
    end case;
  end Turn_Right;

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

  procedure Skip_till_Space (f : in out HAT.File_Type; times : Positive) is
    c : Character;
    use HAT;
  begin
    for repeat in 1 .. times loop
      while not End_Of_File (f) loop
        Get (f, c);
        exit when c = ' ';
      end loop;
      exit when End_Of_File (f);
    end loop;
  end Skip_till_Space;

  function Sgn_64 (i : Integer_64) return Integer_64 is
  begin
    if i > 0 then
      return 1;
    elsif i < 0 then
      return -1;
    else
      return 0;
    end if;
  end Sgn_64;

  function Image (i : Integer_64) return HAT.VString is
    use HAT;
    res : VString := +i'Image;
  begin
    if i < 0 then
      return res;
    else
      --  Remove the leading ' '.
      Delete (res, 1, 1);
      return res;
    end if;
  end Image;

  function Deg_2_Rad (a : HAT.Real) return HAT.Real is
    use HAT;
  begin
    return (Pi / 180.0) * a;
  end Deg_2_Rad;

  function Sim_XOR (a, b : Integer_64) return Integer_64 is
    x : Integer_64 := a;
    y : Integer_64 := b;
    res : Integer_64 := 0;
    m : Integer_64 := 1;
    x1, y1 : Integer_64;
  begin
    if a < 0 or else b < 0 then
      HAT.Put ("XOR on negative values!");
    end if;
    while x > 0 or else y > 0 loop
      x1 := x mod 2;
      y1 := y mod 2;
      if (x1 = 1 or y1 = 1) and then not (x1 = 1 and y1 = 1) then
        res := res + m;
      end if;
      x := x / 2;
      y := y / 2;
      m := m * 2;
    end loop;
    return res;
  end Sim_XOR;

  package body Hash_Maps is

    --  Hash map code extended from AoC_2023_15's implementation.
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
       new_value : in     Integer_64;
       replace   : in     Boolean;      --  Replace existing value ?
       value     :    out Integer_64)   --  If key exists, we get previous value.
    is
      b : constant Natural := HASH (key);
      new_slots_total : Natural;
      use HAT;
    begin
      for s in 1 .. hm (b).slots loop
        if hm (b).slot (s).key = key then
          value := hm (b).slot (s).value;
          if replace then
            hm (b).slot (s).value := new_value;
          end if;
          return;
        end if;
      end loop;
      --  Append new value.
      new_slots_total := hm (b).slots + 1;
      hm (b).slot (new_slots_total).key   := key;
      hm (b).slot (new_slots_total).value := new_value;
      hm (b).slots := new_slots_total;
      value := new_value;
    end Insert;

    procedure Find
      (hm              : in out Hash_Map_Type;
       key             : in     HAT.VString;
       not_found_value : in     Integer_64;
       value           :    out Integer_64)
    is
      b : constant Natural := HASH (key);
      use HAT;
    begin
      for s in 1 .. hm (b).slots loop
        if hm (b).slot (s).key = key then
          value := hm (b).slot (s).value;
          return;
        end if;
      end loop;
      value := not_found_value;
    end Find;

  end Hash_Maps;

end AoC_Toolbox;
