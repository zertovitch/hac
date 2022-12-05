--  Solution to Advent of Code 2022, Day 5
------------------------------------------
--  $ puzzle title here! $
--
--  https://adventofcode.com/2022/day/5
--  Copy of questions in: aoc_2022_05_questions.txt

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
with HAT;

--  --  Interfaces is needed for compiling on both
--  --  HAC and GNAT (64-bit integer: Integer_64):
--  with Interfaces;

procedure AoC_2022_05 is
  --  use HAT, Interfaces;
  use HAT;

  verbose : constant Boolean := True;
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer;

  c, sep, bracket : Character;
  move : String (1 .. 4);
  from : String (1 .. 5);
  to   : String (1 .. 4);
  a, b, n : Integer;
  f : File_Type;

  type Storage is array (1..100) of Character;
  
  type Stack is record
    top : Natural;
    s   : Storage;
  end record;
  
  s : array (1 .. 9) of Stack;
  
  name : VString := +"aoc_2022_05.txt";

  h : Natural;

  procedure Show is
  begin
    for y in reverse 1 .. Storage'Last loop
      for x in s'Range loop
        if y > s(x).top then
          put("... ");
        else
          put(+'[' & s(x).s(y) & "] ");
        end if;
      end loop;
      new_line;
    end loop;
  end;
  
begin
  r (1) := 0;
  r (2) := 0;
  for part in 1 .. 2 loop
    for x in s'Range loop
      s(x).top := 0;
    end loop;
    h := 0;
    Open (f, name);
    while not End_Of_File (f) loop
      Get (f, sep);
      Get (f, c);
      exit when c in '1' .. '9';
      h := h + 1;
      Skip_Line(f);
    end loop;
    Close (f);
    put(h);
    Open (f, name);
    for y in reverse 1 .. h loop
      for x in s'Range loop
        Get (f, bracket);
        Get (f, c);
        s (x).s(y):= c;
        if c in 'A' .. 'Z' then
          s(x).top := Max (s(x).top, y);
        end if;
        Get (f, bracket);
        if x <s'Last then
          Get (f, sep);
        end if;
      end loop;
    end loop;
    new_line;
    show;
    skip_line(f,2);    
    while not End_Of_File (f) loop
      Get (f, move);
      Get (f, n); put(+"n="&n);
      Get (f, from);
      Get (f, a); put(+"a="&a);
      Get (f, to);
      Get (f, b); put(+"b="&b);
      new_line;
      if part = 1 then
        for count in 1 .. n loop
          s(b).s (s(b).top+1):= s(a).s (s(a).top);
          s(a).top := s(a).top - 1;
          s(b).top := s(b).top+1;
        end loop;
      else
        for count in 1 .. n loop
          s(b).s (s(b).top+count):= s(a).s (s(a).top-n+count);
        end loop;
          s(a).top := s(a).top - n;
          s(b).top := s(b).top+n;
      end if;
    end loop;
    Close (f);
    show;
    r (part) := 0;
  end loop;
  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer'Value (To_String (Argument (1))) or
       r (2) /= Integer'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: bla bla:" & Integer'Image (r (1)));
    Put_Line (+"Part 2: bli bli:" & Integer'Image (r (2)));
    --  Part 1: validated by AoC: 
    --  Part 2: validated by AoC: 
  end if;
end AoC_2022_05;
