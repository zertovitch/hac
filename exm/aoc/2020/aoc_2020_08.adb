--  Solution to Advent of Code 2020, Day 8
------------------------------------------
--  Handheld Halting
--
--  https://adventofcode.com/2020/day/8
--
--
--  HAC 0.081 version.
--
--  HAC 0.081 "nice to have"'s detected in this exercise:
--
--    *     I/O with enums, at least 'Image
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_08 is
  type Instr is (acc, jmp, nop);
  max : constant := 1000;
  code : array (1 .. max) of Instr;
  oper : array (1 .. max) of Integer;
  last : Natural := 0;
  --
  procedure Run (verbose : Boolean) is
    seen : array (1 .. max) of Boolean;
    a : Integer := 0;
    c : Integer := 1;
    ok : Boolean;
  begin
    --
    for x in 1 .. last loop
      seen (x) := False;
    end loop;
    while c <= last loop
      seen (c) := True;
      case code (c) is
        when nop =>  c := c + 1;
        when acc => a := a + oper (c); c := c + 1;
        when jmp => c := c + oper (c);
      end case;
      exit when seen (c);
    end loop;
    ok := c > last;
    if verbose or ok then
       Put (+"Acc = " & a);
      if ok then Put_Line (" OK!"); else Put_Line (" infinite loop"); end if;
    end if;
  end Run;
  --
  procedure Swap (c : Integer) is
  begin
    case code (c) is
      when nop => code (c) := jmp;
      when acc => null;
      when jmp => code (c) := nop;
    end case;
  end Swap;
  c1, c2, c3 : Character;
  i : Instr;
  v : Integer;
  f : File_Type;
begin
  Open (f, "aoc_2020_08.txt");
  while not End_Of_File (f) loop
    Get (f, c1);
    Get (f, c2);
    Get (f, c3);
    if +"" & c1 & c2 & c3 = "acc" then i := acc; end if;
    if +"" & c1 & c2 & c3 = "jmp" then i := jmp; end if;
    if +"" & c1 & c2 & c3 = "nop" then i := nop; end if;
    Get (f, v);
    last := last + 1;
    code (last) := i;
    oper (last) := v;
  end loop;
  Close (f);
  Put_Line (+"Instructions: " & last);
  --
  Run (True);
  --
  --  Fix the code:
  --
  for c in 1 .. last loop
    Swap (c);
    Run (False);
    Swap (c);
  end loop;
end AoC_2020_08;
