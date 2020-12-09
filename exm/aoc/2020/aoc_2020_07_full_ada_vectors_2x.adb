--  Solution to Advent of Code 2020, Day 7
------------------------------------------
--  Handy Haversacks
--
--  https://adventofcode.com/2020/day/7
--
--  Full Ada version.
--  Rules as Vector's, Vector of Contained_Spec each rule.
--
with Ada.Containers.Vectors,
     Ada.Text_IO;

procedure AoC_2020_07_full_Ada_Vectors_2x is
  use Ada.Text_IO;

  type Base_Colour is
    (aqua, beige, black, blue, bronze, brown, chartreuse, coral, crimson, cyan, fuchsia, gold,
     gray, green, indigo, lavender, lime, magenta, maroon, olive, orange, plum, purple, red,
     salmon, silver, tan, teal, tomato, turquoise, violet, white, yellow);

  type Colour_Qualifier is
    (bright, clear, dark, dim, dotted, drab, dull, faded, light,
     mirrored, muted, other, pale, plaid, posh, shiny, striped, vibrant, wavy);

  package BCIO is new Enumeration_IO (Base_Colour);
  package CQIO is new Enumeration_IO (Colour_Qualifier);
  package IIO is new Integer_IO (Integer);
  use BCIO, CQIO, IIO;

  type Colour is record
    cq : Colour_Qualifier;
    bc : Base_Colour;
  end record;

  type Contained_Spec is record
    col : Colour;
    num : Natural;
  end record;

  package Contains_Pkg is new Ada.Containers.Vectors (Positive, Contained_Spec);

  type Rule is record
    container   : Colour;
    contains    : Contains_Pkg.Vector;
    can_contain : Boolean;  --  Used for marking container bag rule without double-counting.
  end record;

  package Rules_Pkg is new Ada.Containers.Vectors (Positive, Rule);

  rules : Rules_Pkg.Vector;

  details : constant Boolean := False;

  procedure Put (c : Colour) is
  begin
    Put (c.cq); Put ("~"); Put (c.bc);
  end Put;

  function Containing_Bags (c : Colour) return Natural is
    --
    procedure Scan_Containing_Bags (c : Colour) is
      is_top : Boolean := True;
    begin
      for r of rules loop
        for ct of r.contains loop
          if ct.col = c then
            is_top := False;
            r.can_contain := True;
            if details then
              Put ("   container of "); Put (c); Put (" is "); Put (r.container); New_Line;
            end if;
            Scan_Containing_Bags (r.container);
          end if;
        end loop;
      end loop;
      if is_top and details then
        Put ("Top: "); Put (c); New_Line;
      end if;
    end Scan_Containing_Bags;
    --
    can_contain_count : Natural := 0;
  begin
    for r of rules loop
      r.can_contain := False;
    end loop;
    Scan_Containing_Bags (c);
    for r of rules loop
      if r.can_contain then
        can_contain_count := can_contain_count + 1;
      end if;
    end loop;
    return can_contain_count;
  end Containing_Bags;

  function Contained_Bags (c : Colour) return Natural is
    contained : Natural := 0;
  begin
    for r of rules loop
      if r.container = c then
        if details then
          Put ("Container "); Put (c); New_Line;
        end if;
        for ct of r.contains loop
          contained := contained +
            ct.num * (1 + Contained_Bags (ct.col));
        end loop;
        exit;
      end if;
    end loop;
    return contained;
  end Contained_Bags;

  procedure Get_Rules is
    f : File_Type;
    sep : Character;
    new_rule : Rule;
    new_spec : Contained_Spec;
    bags_contain : String := " bags contain";
  begin
    Open (f, In_File, "aoc_2020_07.txt");
    while not End_Of_File (f) loop
      Get (f, new_rule.container.cq);
      Get (f, new_rule.container.bc);
      if details then
        Put (new_rule.container);
      end if;
      Get (f, bags_contain);
      if details then
        Put (bags_contain & ": ");
      end if;
      --  If you forget the following, the executable built by
      --  GNAT for Win64, Win32 breaks without STORAGE_ERROR.
      new_rule.contains.Clear;
      loop
        begin
          Get (f, new_spec.num);
        exception
          when Data_Error =>  --  "no other bags"
            Skip_Line (f);
            exit;
        end;
        Get (f, new_spec.col.cq);
        Get (f, new_spec.col.bc);
        if details then
          Put (new_spec.num, 2);
          Put (" ");
          Put (new_spec.col);
          Put ("   ");
        end if;
        new_rule.contains.Append (new_spec);
        loop
          Get (f, sep);
          exit when sep = ',' or sep = '.';
        end loop;
        exit when End_Of_Line (f);
      end loop;
      if details then
        New_Line;
      end if;
      rules.Append (new_rule);
    end loop;
    Close (f);
  end Get_Rules;
  --
begin
  Get_Rules;
  Put_Line ("Rules about bag contents:" & rules.Length'Image);
  Put_Line ("Part 1: all possible direct or indirect containers of shiny gold:" &
              Containing_Bags ((shiny, gold))'Image);
  Put_Line ("Part 2: all bags contained by shiny gold:" &
              Contained_Bags ((shiny, gold))'Image);
  --  Rules: 594
  --  Part 1: all possible direct or indirect containers of shiny gold: 169
  --  Part 2: all bags contained by shiny gold: 82372
end AoC_2020_07_full_Ada_Vectors_2x;
