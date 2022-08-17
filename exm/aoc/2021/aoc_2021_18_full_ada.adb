--  Solution to Advent of Code 2021, Day 18
-------------------------------------------
--  Snailfish
--
--  NB: this doesn't compile with HAC, only with a "full Ada" compiler.
--
--  To do: make a HAC solution using a heap structure for the
--  binary tree (idea Maxim Reznik)

--  https://adventofcode.com/2021/day/18
--  Copy of questions in: aoc_2021_18_questions.txt
--
with HAT;  --  For a build with "full Ada": files HAT*.ad* are in ../../../src

with Ada.Unchecked_Deallocation;

procedure AoC_2021_18_full_Ada is
  use HAT;

  type Node_Type;

  type Node_Access is access Node_Type;

  type Node_Type (is_pair : Boolean) is record
    case is_pair is
      when True  => left, right : Node_Access;
      when False => number      : Integer;
    end case;
  end record;

  procedure Free is new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

  procedure Clear (n : in out Node_Access) is
  begin
    if n /= null then
      if n.is_pair then
        Clear (n.left);
        Clear (n.right);
      end if;
      Free (n);
    end if;
  end Clear;

  subtype Snailfish_Number is Node_Access;  --  Just a nicer name.

  procedure Show (node : Snailfish_Number) is
  begin
    if node.is_pair then
      Put ('[');
      Show (node.left);
      Put (',');
      Show (node.right);
      Put (']');
    else
      Put (node.number, 0);
    end if;
  end Show;
  pragma Unreferenced (Show);

  procedure Reduce (number : in out Snailfish_Number) is
    change : Boolean;
    add_left, add_right : Natural;
    --
    procedure Solve_Left (node : Snailfish_Number)  is
    begin
      if add_left = 0 then
        return;
      end if;
      if node.is_pair then
        --  The left exploded number goes rightmost, on the left side:
        Solve_Left (node.right);    --  Prioritize the right node.
        Solve_Left (node.left);
      elsif add_left > 0 then
        node.number := node.number + add_left;
        add_left := 0;
      end if;
    end Solve_Left;
    --
    procedure Solve_Right (node : Snailfish_Number)  is
    begin
      if add_right = 0 then
        return;
      end if;
      if node.is_pair then
        --  The right exploded number goes leftmost, on the right side:
        Solve_Right (node.left);    --  Prioritize the left node.
        Solve_Right (node.right);
      elsif add_right > 0 then
        node.number := node.number + add_right;
        add_right := 0;
      end if;
    end Solve_Right;
    --
    procedure Explode (node : in out Snailfish_Number; level : Natural)  is
    begin
      if change then
        return;
      end if;
      if node.is_pair then
        if level = 5 then
          pragma Assert (not node.left.is_pair);
          pragma Assert (not node.right.is_pair);
          add_left  := node.left.number;
          add_right := node.right.number;
          Free (node);
          node := new Node_Type'(False, 0);
          change := True;
        else
          Explode (node.left, level + 1);
          if change then
            --  There is always a number in the part of the tree
            --  starting at the sibling node. So, no need to try
            --  adding the number elsewhere in the tree.
            Solve_Right (node.right);
          else
            Explode (node.right, level + 1);
            if change then
              --  Same remark as above.
              Solve_Left (node.left);
            end if;
          end if;
        end if;
      end if;
    end Explode;
    --
    procedure Split (node : in out Snailfish_Number) is
      new_left, new_right : Snailfish_Number;
    begin
      if change then
        return;
      end if;
      if node.is_pair then
        Split (node.left);
        Split (node.right);
      elsif node.number > 9 then
        new_left  := new Node_Type'(False, node.number / 2);
        new_right := new Node_Type'(False, node.number - new_left.number);
        Free (node);
        node := new Node_Type'(True, new_left, new_right);
        change := True;
      end if;
    end Split;
    --
  begin
    loop
      change := False;
      add_left  := 0;
      add_right := 0;
      Explode (number, 1);
      Split (number);
      exit when not change;
    end loop;
  end Reduce;
  --
  function Magnitude (node : Snailfish_Number) return Integer is
  begin
    if node.is_pair then
      return 3 * Magnitude (node.left) + 2 * Magnitude (node.right);
    else
      return node.number;
    end if;
  end Magnitude;
  --
  function "+" (a, b : Snailfish_Number) return Snailfish_Number is
    a_plus_b : Snailfish_Number;
  begin
    if a = null then
      return b;
    end if;
    if b = null then
      return a;
    end if;
    --  Snailfish addition:
    a_plus_b := new Node_Type'(True, a, b);
    --  "snailfish numbers must always be reduced, and the process
    --     numbers of adding two snailfish numbers can result in
    --     snailfish that need to be reduced."
    --  This rule avoids nesting levels > 5 at any point, including
    --  after the next unreduced addition.
    Reduce (a_plus_b);
    return a_plus_b;
  end "+";
  --
  number_of_terms : Natural := 0;
  root : Snailfish_Number := null;
  --
  procedure Read_Data (
    l1, l2     : Natural := 0;     --  When not 0, read only lines l1, l2.
    is_swapped : Boolean := False
  )
  is
    input : constant VString := +"aoc_2021_18.txt";
    --
    c, sep : Character;
    f : File_Type;
    function Read_Item return Snailfish_Number is
      node : Snailfish_Number;
    begin
      Get (f, c);
      if c = '[' then
        --  It's a pair
        node := new Node_Type (is_pair => True);
        node.left := Read_Item;
        Get (f, sep);  --  ','
        node.right := Read_Item;
        Get (f, sep);  --  ']'
      else
        --  It's a literal
        node := new Node_Type (is_pair => False);
        node.number := Ord (c) - Ord ('0');
      end if;
      return node;
    end Read_Item;
    --
    l : Natural := 0;
    dummy : Snailfish_Number;
  begin
    Open (f, input);
    loop
      l := l + 1;
      if l1 = 0 or else l = l1 or else l = l2 then
        if is_swapped then
          root := Read_Item + root;
        else
          root := root + Read_Item;
        end if;
      else
        dummy := Read_Item;
        Clear (dummy);
      end if;
      exit when End_Of_File (f);
    end loop;
    Close (f);
    number_of_terms := l;
  end Read_Data;
  --
  max : Natural := 0;
  r : array (1 .. 2) of Integer;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  --
  r (1) := Magnitude (root);
  Clear (root);
  for i in 1 .. number_of_terms - 1 loop
    for j in i + 1 .. number_of_terms loop
      for is_swapped in Boolean loop
        --  Re-reading selectively the data is simpler
        --  than deep-cloning trees.
        Read_Data (i, j, is_swapped);
        max := Integer'Max (max,  Magnitude (root));
        Clear (root);
      end loop;
    end loop;
  end loop;
  r (2) := max;
  if compiler_test_mode then
    if r (1) /= Integer_Value (Argument (1)) or
       r (2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: Magnitude of the sum of all terms                : " & r (1));
    Put_Line (+"Part 2: Largest magnitude of the sum of any pair of terms: " & r (2));
    --  Part 1: validated by AoC: 3665.
    --  Part 2: validated by AoC: 4775.
  end if;
end AoC_2021_18_full_Ada;
