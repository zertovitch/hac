--  Solution to Advent of Code 2020, Day 8
------------------------------------------
--  Handheld Halting
--
--  https://adventofcode.com/2020/day/8
--
--  HAC 0.085 version.
--
--  HAC 0.085 "nice to have"'s detected in this exercise:
--
--    *     I/O with enums, at least 'Image
--    *     `  seen := (others => False);  `
--
with HAT; use HAT;  --  in ../../../src

procedure AoC_2020_08 is
  subtype Machine_Code_Range is Positive range 1 .. 1000;
  type Instr is (acc, jmp, nop);
  code : array (Machine_Code_Range) of Instr;
  oper : array (Machine_Code_Range) of Integer;
  last : Natural := 0;
  --
  procedure Run (result : out Integer; ok : out Boolean) is
    seen : array (Machine_Code_Range) of Boolean;
    a : Integer := 0;
    c : Integer := 1;
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
    ok := c > last;  --  Went past the last instruction: program terminated.
    result := a;
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
  function Exit_Diagnostic (x : Boolean) return VString is
  begin
    if x then return +"correct exit!";
         else return +"infinite loop detected.";
    end if;
  end Exit_Diagnostic;
  --
  asm : String (1 .. 3);
  i : Instr := nop;
  v, a1, a2 : Integer;
  f : File_Type;
  done_1, done_2 : Boolean;
  test_mode : constant Boolean := Argument_Count >= 2;
begin
  Open (f, "aoc_2020_08.txt");
  while not End_Of_File (f) loop
    Get (f, asm);
    if asm = "acc" then i := acc; end if;
    if asm = "jmp" then i := jmp; end if;
    if asm = "nop" then i := nop; end if;
    Get (f, v);
    last := last + 1;
    code (last) := i;
    oper (last) := v;
  end loop;
  Close (f);
  if not test_mode then Put_Line (+"Instructions: " & last); end if;
  --
  Run (a1, done_1);
  --
  --  Try fixing the machine code by swapping "nop" and "jmp" instructions:
  --
  for c in 1 .. last loop
    Swap (c);
    Run (a2, done_2);
    exit when done_2;
    Swap (c);
  end loop;
  --
  if test_mode then
    if a1 /= Integer_Value (Argument (1)) or
       a2 /= Integer_Value (Argument (2))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Accumulator = " & a1 & ";  " & Exit_Diagnostic (done_1));
    Put_Line (+"Accumulator = " & a2 & ";  " & Exit_Diagnostic (done_2));
    --  Part 1: validated by AoC: 1394
    --  Part 2: validated by AoC: 1626
  end if;
end AoC_2020_08;
