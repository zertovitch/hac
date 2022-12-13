--  Solution to Advent of Code 2022, Day 13
-------------------------------------------
--  Distress Signal
--
--  https://adventofcode.com/2022/day/13
--  Copy of questions in: aoc_2022_13_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

procedure AoC_2022_13 is
  use HAT;

  type Order is (correct, wrong, undecided);

  --  Here we compare a pair of recursive lists in the form:
  --    [[1],[2,3,4]]
  --    [[1],4]

  function Compare_List (s1, s2 : VString) return Order;

  function Compare_Elements (s1, s2 : VString) return Order is
    i1, i2 : Integer;
    h1, h2 : Character;
  begin
    h1 := Element (s1, 1);
    h2 := Element (s2, 1);
    if h1 in '0' .. '9' and then h2 in '0' .. '9' then
      i1 := Integer_Value (Slice (s1, 1, Length (s1)));
      i2 := Integer_Value (Slice (s2, 1, Length (s2)));
      if i1 < i2 then
        return correct;
      elsif i1 > i2 then
        return wrong;
      else
        return undecided;
      end if;
    elsif h1 = '[' and h2 = '[' then
      return Compare_List (s1, s2);
    elsif h1 = '[' then
      return Compare_List (s1, '[' & s2 & ']');
    else
      return Compare_List ('[' & s1 & ']', s2);
    end if;
  end Compare_Elements;

  function Element_End (s : VString) return Natural is
    l : constant Natural :=  Length (s);
    skip : Natural := 0;
  begin
    if l = 0 then
      return 0;
    end if;
    for i in 1 .. l loop
      case  Element (s, i) is
        when ',' =>
          if skip = 0 then
            return i - 1;
          end if;
        when '[' =>
          skip := skip + 1;
        when ']' =>
          skip := skip - 1;
        when others =>
          null;
      end case;
    end loop;
    return l;
  end Element_End;

  --  Remove first element in "a,b,c"
  procedure Delete_Head (s : in out VString; element_length : Natural) is
  begin
    Delete (s, 1, element_length);
    if Length (s) > 0 and then Element (s, 1) = ',' then
      Delete (s, 1, 1);
    end if;
  end Delete_Head;

  --  Remove then '[' and ']' at both ends.
  procedure Delete_Brackets (s : in out VString) is
  begin
    Delete (s, Length (s), Length (s));
    Delete (s, 1, 1);
  end Delete_Brackets;

  function Compare_List (s1, s2 : VString) return Order is
    e1, e2 : Integer;
    v1, v2 : VString;
    res : Order;
  begin
    v1 := s1;
    v2 := s2;
    --  Remove the outmost brackets around the list.
    Delete_Brackets (v1);
    Delete_Brackets (v2);
    --  Process the elements, if any.
    loop
      e1 := Element_End (v1);
      e2 := Element_End (v2);
      if e1 = 0 and e2 = 0 then
        return undecided;
      elsif e1 = 0 then
        --  "If the left list runs out of items first,
        --   the inputs are in the right order."
        return correct;
      elsif e2 = 0 then
        --  "If the right list runs out of items first,
        --   the inputs are not in the right order."
        return wrong;
      end if;
      res := Compare_Elements (Slice (v1, 1, e1), Slice (v2, 1, e2));
      if res /= undecided then
        return res;
      end if;
      --  Move to next element on both lists.
      Delete_Head (v1, e1);
      Delete_Head (v2, e2);
    end loop;
  end Compare_List;

  --  
  function One_if_lesser (s1, s2 : VString) return Natural is
  begin
    if Compare_List (s1, s2) = correct then
      return 1;
    else
      return 0;
    end if;
  end One_if_lesser;

  f : File_Type;
  s : array (1 .. 2) of VString;

  pair_index, sum_correct_pair_indices, before_2, before_6 : Natural := 0;

  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

begin
  Open (f, "aoc_2022_13.txt");
Read_Data :
  loop
    for i in 1 .. 2 loop
      Get_Line (f, s (i));
      before_2 := before_2 + One_if_lesser (s (i), +"[[2]]");
      before_6 := before_6 + One_if_lesser (s (i), +"[[6]]");
    end loop;
    pair_index := pair_index + 1;
    if Compare_List (s (1), s (2)) = correct then
      sum_correct_pair_indices := sum_correct_pair_indices + pair_index;
    end if;
    exit when End_Of_File (f);
    Skip_Line (f);
  end loop Read_Data;
  Close (f);

  r (1) := sum_correct_pair_indices;
  r (2) := (before_2 + 1) * (before_6 + 2);

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line
      (+"Part 1: sum of indices of list pairs in correct order  : " & r (1));
    Put_Line
      (+"Part 2: product of indices of [[2]] and [[6]] dividers : " & r (2));
    --  Part 1: validated by AoC: 6568
    --  Part 2: validated by AoC: 19493
  end if;
end AoC_2022_13;
