--  Solution to Advent of Code 2020, Day 19
-------------------------------------------
--  Monster Messages
--
--  https://adventofcode.com/2020/day/19
--

with Ada.Text_IO;

with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_19_full_Ada is
  --  n : constant := 6; fn : constant String := "mini.txt";
  n   : constant := 129;
  fn  : constant String := "aoc_2020_19.txt";

  type Rule_Index_List is array (Positive range <>) of Integer;
  submax : constant := 5;

  subtype Rule_Index_List_Fixed is Rule_Index_List (1 .. submax);

  type Rule_Type (is_terminal : Boolean := False) is record
    case is_terminal is
      when True  =>
        leaf : Character;
      when False =>
        max : Integer;
        sub : Rule_Index_List_Fixed;
        alt : Natural;  --  alternative subrule list starts at `alt`
    end case;
  end record;

  max_rule_id : constant := 500;
  rule : array (0 .. max_rule_id) of Rule_Type;
  verbose : constant Boolean := False;
  --
  function Is_Valid (s : String) return Boolean is
    --
    --  We verify the string (s) against the rules (list),
    --  in a LISP-esque fashion.
    --  The nice thing in this problem is the tail recursion
    --  on the string AND on the rule list.
    --
    function Verify (s : String; list : Rule_Index_List) return Boolean is
      tail_rule_list : Rule_Index_List renames list (list'First + 1 .. list'Last);
    begin
      if list'Length = 0 then
        --  OK if the string is empty as well.
        return s'Length = 0;
      end if;
      if s'Length = 0 then
        return False;  --  String is too short, there are rules left.
      end if;
      declare
        r1 : Rule_Type renames rule (list (list'First));
      begin
        if r1.is_terminal then
          return
              s (s'First) = r1.leaf
            and then
              --  Test the rest of the string against the other rules.
              Verify (s (s'First + 1 .. s'Last), tail_rule_list);
        elsif r1.alt = 0 then
          --  Test the sub-rules, appended with the tail of the rules, and
          --  verify all that on the local string.
          return Verify (s, r1.sub (1 .. r1.max) & tail_rule_list);
        else
          --  Test separately both subrule lists:
          return
            Verify (s, r1.sub (1 .. r1.alt - 1)  & tail_rule_list)
               or else
            Verify (s, r1.sub (r1.alt .. r1.max) & tail_rule_list);
        end if;
      end;
    end Verify;
    --
    --  Test rule 0 (and all the rest...):
    is_test_ok : constant Boolean := Verify (s, (1 => 0));
  begin
    if verbose then
      Put_Line ("Tested:  " & s & "   : " & (if is_test_ok then "OK" else "invald"));
    end if;
    return is_test_ok;
  end Is_Valid;
  --
  procedure Parse_Rules (f : in out File_Type) is
    procedure Get_Nat (n : out Natural) is
      c : Character;
    begin
      n := 0;
      Get (f, c);
      while c = ' ' loop  --  Skip heading blanks.
        Get (f, c);
      end loop;
      loop
        exit when (c < '0') or (c > '9');
        n := n * 10 + Ord (c) - Ord ('0');
        Get (f, c);
      end loop;
    end Get_Nat;
    --
    c, sep : Character;
    i, j, k : Integer;
  begin
    for count in 1 .. n loop
      Get_Nat (i);
      Get (f, sep);
      Get (f, c);
      if c = '"' then
        Get (f, c);
        rule (i) := (is_terminal => True, leaf => c);
        Get (f, sep);
        if verbose then
          Put_Line (+"Terminal " & i & " -> " & rule (i).leaf);
        end if;
      else
        k := 0;
        rule (i).alt := 0;
        loop
          if (c >= '0') and (c <= '9') then
            j := Ord (c) - Ord ('0');
          else
            j := 0;
          end if;
          k := k + 1;
          while not End_Of_Line (f) loop
            Get (f, c);
            exit when (c < '0') or (c > '9');
            j := j * 10 + Ord (c) - Ord ('0');
          end loop;
          rule (i).sub (k) := j;
          exit when End_Of_Line (f);
          Get (f, c);
          if c = '|' then
            rule (i).alt := k + 1;
            Get (f, c);
          end if;
        end loop;
        rule (i).max := k;
        if verbose then
          Put (+"  List " & i & ": ");
          for l in 1 .. k loop
            if rule (i).alt = l then Put ("| "); end if;
            Put (+"" & rule (i).sub (l) & " ");
          end loop;
          New_Line;
        end if;
      end if;
    end loop;
  end Parse_Rules;
  --
  total : Natural;
  f : File_Type;
begin
  for part in 1 .. 2 loop
    total := 0;
    Open (f, fn);
    Parse_Rules (f);
    if part = 2 then
      rule (8)  := (is_terminal => False, max => 3, alt => 2, sub => (42, 42,  8, -1, -1));
      rule (11) := (is_terminal => False, max => 5, alt => 3, sub => (42, 31, 42, 11, 31));
    end if;
    Ada.Text_IO.Skip_Line (f, 2);
    while not End_Of_File (f) loop
      if Is_Valid (Ada.Text_IO.Get_Line (f)) then
        total := total + 1;
      end if;
    end loop;
    Close (f);
    Put_Line (total);
    --  Validated by AoC: 226
    --  Validated by AoC: 355
  end loop;
end AoC_2020_19_full_Ada;
