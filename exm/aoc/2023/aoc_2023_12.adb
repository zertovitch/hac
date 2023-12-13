--  Solution to Advent of Code 2023, Day 12
-------------------------------------------
--  Hot Springs
--
--  https://adventofcode.com/2023/day/12
--  Copy of questions in: aoc_2023_12_questions.txt
--
--  HAC 0.26 "nice to have"'s detected in this exercise:
--    *     Replace_Element for VString

with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2023.gpr .
with HAT;

procedure AoC_2023_12 is

  use AoC_Toolbox, HAT;

  --  Each data row has two lists:
  --    - a list of spring conditions
  --    - a list of sizes of contiguous groups of damaged springs
  --
  --  Here we set for HAC a maximum for the second list (HAC knows
  --  only constrained array types):

  max_sizes : constant := 100;

  --  Size of a contiguous group of damaged springs:
  subtype Size_Type is Natural;

  type Size_Array is array (1 .. max_sizes) of Size_Type;

  type Row is record
    code  : VString;     --  No actual limit in storage
    size  : Size_Array;
    sizes : Natural;     --  Number of sizes
  end record;

  map : array (1 .. 1000) of Row;
  top : Natural := 0;

  --  Idea: on a given list of spring conditions of length x
  --  (possibly truncated from the right), and a list of contiguous
  --  groups of damaged springs, of length y (also possibly truncated
  --  from the right), if the contents of the lists are intact, the
  --  result of the problem for the truncated pair is unique,
  --  so we can memoize it.

  max_length : constant := 120;  --  Spring conditions

  type Memo_Type is array (1 .. max_length, 1 .. max_sizes) of Integer;

  memo, zero : Memo_Type;

  unknown : constant := -1;

  --  input_name : constant VString := +"mini";
  input_name : constant VString := +"aoc_2023_12";
  --
  procedure Read_Data is
    c, sep : Character;
    i : Integer;
    f : File_Type;
  begin
    Open (f, input_name & ".txt");
    while not End_Of_File (f) loop
      top := top + 1;
      map (top).code := Null_VString;
      loop
        Get (f, c);
        exit when c = ' ';
        map (top).code := map (top).code & c;
      end loop;
      i := 0;
      loop
        i := i + 1;
        Get (f, map (top).size (i));
        exit when End_Of_Line (f);
        Get (f, sep);
      end loop;
      map (top).sizes := i;
    end loop;
    Close (f);
    for i in memo'Range (1) loop
      for j in memo'Range (2) loop
        zero (i, j) := unknown;
      end loop;
    end loop;
  end Read_Data;

  r : array (Part_Type) of Integer;

  verbose : constant Boolean := False;

  procedure Do_Part (p : Part_Type) is

    --  We erode the pair of lists from right to left, so the
    --  index 1 remains through recursion.

    function Count (r : Row; are_remaining_lists_intact : Boolean) return Natural is
      head : Row;
      --  `head` is a copy of `r` with at least the last item of the spring
      --         condition list removed.
      len : Natural;
      c : Integer;

      procedure Expected_Size_One is
      begin
        if len = 1 then
          --  The current code (condition list) is just a lone "#".
          if r.sizes = 1 then
            --  ^ A single '#' is expected and has been found.
            c :=  1;
          else
            --  This group is OK, but there are other series of "#"
            --  expected left to current one -> not a solution!
            c :=  0;
          end if;
        elsif Element (r.code, len - 1) = '#' then
          c := 0;  --  We have a trailing "##", but size 1 is expected.
        else
          head := r;
          --  We have a possible separator ('?' or '.') left
          --  to the len-th element.
          --  Fine, just drop last element of the list.
          head.sizes := head.sizes - 1;
          --  Remove the separator and the last '#'.
          Delete (head.code, len - 1, len);
          --  We do a recursive call with the knowledge that
          --  contents of both shortened lists are again intact.
          c :=  Count (head, True);
        end if;
      end Expected_Size_One;

      procedure Expected_Size_Two_or_More is
      begin
        if len = 1 then
          --  The current code (condition list) is just a lone "#",
          --  but we expect >= 2 of them, only in the current group!
          --  -> Not a solution!
          c := 0;
        else
          case Element (r.code, len - 1) is
            when '?' =>
              head := r;
              --  Shorten expected string of ###'s by one '#'.
              head.size (head.sizes) := head.size (head.sizes) - 1;
              --  Exclude the '?' = '.' case since 2 or more
              --  '#' are expected in current group. Concretely,
              --  the trailing "?#" becomes "##".
              Delete (head.code, len - 1, len);
              head.code := head.code & '#';
              c := Count (head, False);
            when '.' =>
              --  Element of size >= 2 expected, but size 1 found.
              c := 0;
            when '#' =>
              head := r;
              Delete (head.code, len, len);
              --  Shorten expected string of ###'s by one '#'.
              head.size (head.sizes) := head.size (head.sizes) - 1;
              c := Count (head, False);
            when others =>
              Put ("??? [1]");
          end case;
        end if;
      end Expected_Size_Two_or_More;

    begin
      len := Length (r.code);
      if len = 0 then
        if r.sizes = 0 then
          --  List exhausted
          return 1;
        else
          return 0;
        end if;
      end if;
      --  From here we know that length l is >= 1.
      if are_remaining_lists_intact and then r.sizes > 0 then
        c := memo (len, r.sizes);
        if c /= unknown then
          return c;
        end if;
      end if;
      case Element (r.code, len) is
        when '.' =>
          head := r;
          Delete (head.code, len, len);
          --  The last '.' (an operational spring) can be removed
          --  without changing the result.
          c := Count (head, are_remaining_lists_intact);
        when '?' =>
          head := r;
          Delete (head.code, len, len);
          --  Cases when '?' is a '.' :
          c := Count (head, are_remaining_lists_intact);
          head.code := head.code & '#';
          --  Cases when '?' is a '#' :
          c := c + Count (head, False);
        when '#' =>
          if r.sizes = 0 then
            --  We have at least one guy in the condition list,
            --  but the size list is exhausted -> not a solution.
            c := 0;
          elsif r.size (r.sizes) = 1 then
            --  Last element on the size list is a size = 1.
            Expected_Size_One;
          else
            --  Last element is a size > 1.
            Expected_Size_Two_or_More;
          end if;
        when others =>
          Put ("??? [2]");
      end case;
      if verbose then
        Put (r.code & "  ");
        for i in 1 .. r.sizes loop
          Put (r.size (i), 0); Put (',');
        end loop;
        Put ("   comb:"); Put (c, 0); New_Line;
      end if;
      if are_remaining_lists_intact and then r.sizes > 0 then
        memo (len, r.sizes) := c;
      end if;
      return c;
    end Count;

  begin
    r (p) := 0;
    for line in 1 .. top loop
      memo := zero;
      r (p) := r (p) + Count (map (line), True);
      if verbose then
        Put_Line ("-------------------------------------");
      end if;
    end loop;
  end Do_Part;

  procedure Prepare_Part_2 is
    orig : VString;
  begin
    for line in 1 .. top loop
      --  Apply the unfolding, literally, as explained:
      --    - replace the list of spring conditions with five copies
      --      of itself (separated by ?)
      orig := map (line).code;
      map (line).code := 4 * (orig & '?') & orig;
      --    - replace the list of contiguous groups of damaged springs
      --      with five copies of itself.
      for i in 1 .. 4 loop
        for j in 1 .. map (line).sizes loop
          map (line).size (map (line).sizes * i + j) := map (line).size (j);
        end loop;
      end loop;
      map (line).sizes := map (line).sizes * 5;
    end loop;
  end Prepare_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  T0 : constant Time := Clock;
begin
  Read_Data;
  Do_Part (part_1);
  Prepare_Part_2;
  Do_Part (part_2);
  if compiler_test_mode then
    if r (part_1) /= Integer_Value (Argument (1)) or
       r (part_2) /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: : " & r (part_1));
    Put_Line (+"Part 2: : " & r (part_2));
    --  Part 1: validated by AoC: 7007
    --  Part 2: validated by AoC: 3476169006222
  end if;
end AoC_2023_12;
