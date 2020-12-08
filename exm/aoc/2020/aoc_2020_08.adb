--  Solution to Advent of Code 2020, Day 8
------------------------------------------
--  Handheld Halting
--
--  https://adventofcode.com/2020/day/8
--
--  HAC 0.081 version.
--
--  HAC 0.081 "nice to have"'s detected in this exercise:
--
--    *     I/O with enums, at least 'Image
--    *     `  seen := (others => False);  `
--    *     Get with String parameter, as in `  Get (f, asm);  `
--    *     String-to-literal comparison
--             as in `  if asm = "acc" then i := acc; end if;  `
--
with HAC_Pack;  use HAC_Pack;

procedure AoC_2020_08 is
  subtype Machine_Code_Range is Positive range 1 .. 1000;
  type Instr is (acc, jmp, nop);
  code : array (Machine_Code_Range) of Instr;
  oper : array (Machine_Code_Range) of Integer;
  last : Natural := 0;
  --
  procedure Run (verbose : Boolean) is
    seen : array (Machine_Code_Range) of Boolean;
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
        when nop => c := c + 1;
        when acc => a := a + oper (c); c := c + 1;
        when jmp => c := c + oper (c);
      end case;
      exit when seen (c);
    end loop;
    ok := c > last;
    if verbose or ok then
      Put (+"Accumulator = " & a & ";  ");
      if ok then Put_Line ("correct exit!");
            else Put_Line ("infinite loop.");
      end if;
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
  --
  asm : String (1 .. 3);
  i : Instr;
  v : Integer;
  f : File_Type;
begin
  Open (f, "aoc_2020_08.txt");
  while not End_Of_File (f) loop
    Get (f, asm (1));
    Get (f, asm (2));
    Get (f, asm (3));
    if +asm = "acc" then i := acc; end if;
    if +asm = "jmp" then i := jmp; end if;
    if +asm = "nop" then i := nop; end if;
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
  --  Try fixing the machine code:
  --
  for c in 1 .. last loop
    Swap (c);
    Run (False);
    Swap (c);
  end loop;
end AoC_2020_08;
