--  Solution to Advent of Code 2020, Day 7
------------------------------------------
--  Handy Haversacks
--
--  https://adventofcode.com/2020/day/7
--
--  HAC 0.081 version.
--
--  HAC 0.081 "nice to have"'s detected in this exercise:
--
--    *     I/O with enums, at least 'Image and 'Value. [Solved with HAC v.0.099]
--    *     "=" for composite types ( ` rules (ri).contains (cti).col = c  `)
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_07 is

  type Base_Colour is
    (aqua, beige, black, blue, bronze, brown, chartreuse, coral, crimson, cyan, fuchsia, gold,
     gray, green, indigo, lavender, lime, magenta, maroon, olive, orange, plum, purple, red,
     salmon, silver, tan, teal, tomato, turquoise, violet, white, yellow);

  type Colour_Qualifier is
    (bright, clear, dark, dim, dotted, drab, dull, faded, light,
     mirrored, muted, other, pale, plaid, posh, shiny, striped, vibrant, wavy);

  --  Emulate full Ada's Enumeration_IO's Get (ouch!) ...

  procedure Get_CQ (f : in out File_Type; cq : out Colour_Qualifier) is
    c : Character;
    s : VString;
  begin
    loop
      Get (f, c);
      exit when c < 'a' or c > 'z';
      s := s & c;
    end loop;
    cq := Colour_Qualifier'Value (To_String (s));
    --  We don't treat the invalid cases...
  end Get_CQ;

  procedure Get_BC (f : in out File_Type; bc : out Base_Colour) is
    c : Character;
    s : VString;
  begin
    loop
      Get (f, c);
      exit when c < 'a' or c > 'z';
      s := s & c;
    end loop;
    bc := Base_Colour'Value (To_String (s));
    --  We don't treat the invalid cases...
  end Get_BC;

  procedure Get_Nat (f : in out File_Type; n : out Natural) is
    c : Character;
  begin
    n := 0;
    Get (f, c);
    while c = ' ' loop  --  Skip heading blanks.
      Get (f, c);
    end loop;
    loop
      exit when c < '0' or c > '9';
      n := n * 10 + Ord (c) - Ord ('0');
      Get (f, c);
    end loop;
    --  Result 0 when invalid...
  end Get_Nat;

  type Colour is record
    cq : Colour_Qualifier;
    bc : Base_Colour;
  end record;

  type Contained_Spec is record
    col : Colour;
    num : Natural;
  end record;

  subtype Contains_Range is Integer range 1 .. 4;

  type Contains_List is array (Contains_Range) of Contained_Spec;

  type Rule is record
    container   : Colour;
    num_contain : Natural;
    contains    : Contains_List;
    can_contain : Boolean;  --  Used for marking container bag rule without double-counting.
  end record;

  subtype Rules_Range is Integer range 1 .. 1000;

  rules : array (Rules_Range) of Rule;  --  Cheap version of Vectors...
  rules_count : Natural := 0;

  details : constant Boolean := False;

  procedure Put_Colour (c : Colour) is
  begin
    Put (Colour_Qualifier'Image (c.cq)); Put ("~"); Put (Base_Colour'Image (c.bc));
  end Put_Colour;

  function Equal (c1, c2 : Colour) return Boolean is
  begin
    return c1.cq = c2.cq and c1.bc = c2.bc;
  end Equal;

  function Containing_Bags (c : Colour) return Natural is
    --
    procedure Scan_Containing_Bags (c : Colour) is
      is_top : Boolean := True;
    begin
      for ri in 1 .. rules_count loop
        for cti in 1 .. rules (ri).num_contain loop
          if Equal (rules (ri).contains (cti).col, c) then
            --  Colour c is contained in bag described by rule #ri.
            is_top := False;
            rules (ri).can_contain := True;
            if details then
              Put ("   container of "); Put_Colour (c);
              Put (" is "); Put_Colour (rules (ri).container); New_Line;
            end if;
            Scan_Containing_Bags (rules (ri).container);
          end if;
        end loop;
      end loop;
      if is_top and details then
        Put ("Top: "); Put_Colour (c); New_Line;
      end if;
    end Scan_Containing_Bags;
    --
    can_contain_count : Natural := 0;
  begin
    for ri in 1 .. rules_count loop
      rules (ri).can_contain := False;
    end loop;
    Scan_Containing_Bags (c);
    for ri in 1 .. rules_count loop
      if rules (ri).can_contain then
        can_contain_count := can_contain_count + 1;
      end if;
    end loop;
    return can_contain_count;
  end Containing_Bags;

  function Contained_Bags (c : Colour) return Natural is
    contained : Natural := 0;
  begin
    for ri in 1 .. rules_count loop
      if Equal (rules (ri).container, c) then
        if details then
          Put ("Container "); Put_Colour (c); New_Line;
        end if;
        for cti in 1 .. rules (ri).num_contain loop
          contained := contained +
            rules (ri).contains (cti).num * (1 + Contained_Bags (rules (ri).contains (cti).col));
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
  begin
    Open (f, "aoc_2020_07.txt");
    while not End_Of_File (f) loop
      Get_CQ (f, new_rule.container.cq);
      Get_BC (f, new_rule.container.bc);
      if details then
        Put_Colour (new_rule.container);
      end if;
      for i in 1 .. 13 loop Get (f, sep); end loop;  --  " bags contain"
      if details then
        Put (" bags contain: ");
      end if;
      new_rule.num_contain := 0;
      loop
        Get_Nat (f, new_spec.num);
        if new_spec.num = 0 then
          Skip_Line (f);
          exit;
        end if;
        Get_CQ (f, new_spec.col.cq);
        Get_BC (f, new_spec.col.bc);
        if details then
          Put (new_spec.num, 2);
          Put (" ");
          Put_Colour (new_spec.col);
          Put ("   ");
        end if;
        new_rule.num_contain := new_rule.num_contain + 1;
        new_rule.contains (new_rule.num_contain) := new_spec;
        loop
          Get (f, sep);
          exit when sep = ',' or sep = '.';
        end loop;
        exit when End_Of_Line (f);
      end loop;
      if details then
        New_Line;
      end if;
      rules_count := rules_count + 1;
      rules (rules_count) := new_rule;
    end loop;
    Close (f);
  end Get_Rules;
  --
  shiny_gold : Colour;
  test_mode : constant Boolean := Argument_Count >= 2;
begin
  shiny_gold.cq := shiny;
  shiny_gold.bc := gold;
  Get_Rules;
  if test_mode then
    if (Containing_Bags (shiny_gold) /= Integer_Value (Argument (1))) or
       (Contained_Bags (shiny_gold) /= Integer_Value (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Rules about bag contents: " & rules_count);
    Put_Line (+"Part 1: all possible direct or indirect containers of shiny gold: " &
                Containing_Bags (shiny_gold));
    Put_Line (+"Part 2: all bags contained by shiny gold: " &
                Contained_Bags (shiny_gold));
  end if;
  --  Rules: 594
  --  Part 1: all possible direct or indirect containers of shiny gold: 169
  --  Part 2: all bags contained by shiny gold: 82372
end AoC_2020_07;
