--  Solution to Advent of Code 2024, Day 17
-------------------------------------------
--  Chronospatial Computer
--
--  https://adventofcode.com/2024/day/17
--  Copy of questions in: aoc_2024_17_questions.txt
--
--  HAC 0.40 "nice-to-have"'s detected in this exercise:
--
--    *     Modular types (esp. Unsigned_X and bitwise operators)
--
--  Related links:
--    https://forum.ada-lang.io/
--    https://www.reddit.com/r/adventofcode/

--  HAC 0.40 "nice to have"'s detected in this exercise:
--    *     Unsigned_X and bitwise operators on it.

--  The files aoc_toolbox.ad* are located in ..
--!hac_add_to_path ..
--
with AoC_Toolbox;

--  For building this program with a "full Ada" compiler,
--  such as GNAT, you need the explicit version of the HAT package.
--  The files hat*.ad* are located in ../../../src
--  See also the GNAT project file aoc_2024.gpr .
with HAT;

with Interfaces;

procedure AoC_2024_17 is

  use AoC_Toolbox, HAT, Interfaces;

  subtype UInt is Integer_64;             --  Hack for HAC. "Full Ada": type UInt is mod 2 ** 64;
  subtype Word is Integer range 0 .. 7;   --  Hack for HAC. "Full Ada": type Word is mod 2 ** 3;

  r : array (Part_Type) of VString;

  type Instruction is (adv, bxl, bst, jnz, bxc, ovt, bdv, cdv);

  type Word_Array is array (0 .. 20) of Word;

  type Word_Vect is record
    oct  : Word_Array;
    last : Integer;
  end record;

  procedure Convert (prog_s : in VString; prog : out Word_Vect) is
  begin
    prog.last := Length (prog_s) - 1;
    for i in 1 .. prog.last + 1 loop
      prog.oct (i - 1) := Word (Ord (Element (prog_s, i)) - Ord ('0'));
    end loop;
  end Convert;

  procedure Run (A_start : in UInt; prog : in Word_Vect; code : out Word_Vect) is
    A : UInt := A_start;
    B : UInt := 0;
    C : UInt := 0;
    p : Natural := 0;

    inst : Instruction;
    oper : Word;

    function Combo return UInt is
    begin
      case oper is
        when 0 .. 3 => return UInt (oper);
        when 4      => return A;
        when 5      => return B;
        when 6      => return C;
        when 7      => Put ("Error"); return 0;  --  "Full Ada": raise Program_Error;
      end case;
    end Combo;

  begin
    code.last := -1;

    while p <= prog.last loop
      inst := Instruction'Val (prog.oct (p));
      oper := prog.oct (p + 1);
      p := p + 2;
      case inst is
        when adv => A := A / (2 ** Natural (Combo));
        when bdv => B := A / (2 ** Natural (Combo));
        when cdv => C := A / (2 ** Natural (Combo));
        when bxl => B := Sim_XOR (B, UInt (oper));  --  "Full Ada" has `xor` of course.
        when bst => B := Combo mod 8;
        when jnz => if A /= 0 then p := Natural (oper); end if;
        when bxc => B := Sim_XOR (B, C);  --  "Full Ada" has `xor` of course.
        when ovt =>
          code.last := code.last + 1;
          code.oct (code.last) := Word (Combo mod 8);
      end case;
    end loop;

    --  mini  : result is: 4,6,3,5,6,3,5,2,1,0
    --  input : result is: 5,1,4,0,5,1,0,2,6
  end Run;

  procedure Run_Hardcoded (A_start : in UInt; code : out Word_Vect) is
    A : UInt := A_start;
    B : UInt := 0;
    C : UInt := 0;
  begin
    code.last := -1;
    loop
      B := A mod 8;
      B := Sim_XOR (B, 1);  --  "Full Ada" has `xor` of course.
      C := A / 2 ** Natural (B);
      B := Sim_XOR (B, 4);  --  "Full Ada" has `xor` of course.
      A := A / 8;
      B := Sim_XOR (B, C);  --  "Full Ada" has `xor` of course.
      code.last := code.last + 1;
      code.oct (code.last) := Word (B mod 8);
      exit when A = 0;
    end loop;
  end Run_Hardcoded;

  procedure Do_Part_1 (A_start : UInt; prog_s : VString) is
    prog, res : Word_Vect;
  begin
    r (part_1) := +"";
    Convert (prog_s, prog);
    Run (A_start, prog, res);
    for i in 0 .. res.last loop
      r (part_1) := r (part_1) & Character'Val (res.oct (i) + Character'Pos ('0'));
    end loop;
  end Do_Part_1;

  --  Emulate Full Ada's `p1.oct (from .. to) = p2.oct (from .. to)`
  --
  function Equal_Slice (p1, p2 : Word_Vect; from, to : Natural) return Boolean is
  begin
    if p1.last < to or else p2.last < to then
      return False;
    end if;
    for i in from .. to loop
      if p1.oct (i) /= p2.oct (i) then
        return False;
      end if;
    end loop;
    return True;
  end Equal_Slice;

  procedure Do_Part_2 (prog_s : VString; shortcut : Boolean) is

    prog : Word_Vect;
    best : UInt := UInt'Last;

    procedure Match (base : UInt; d : Natural) is
      code : Word_Vect;
      x : UInt;
    begin
      for i in reverse 0 .. 1023 loop
        x := base + UInt (i) * 8 ** d;
        if shortcut then
          Run_Hardcoded (x, code);
        else
          Run (x, prog, code);
        end if;
        if d <= code.last and then Equal_Slice (code, prog, 0, d) then
          --  if code.last = 15 then
          --    Put (+"Prog: " & prog_s & "; A=" & x'Image & "; d =" & d'Image &
          --         "; code.last =" & code.last'Image & " -> ");
          --    for p in 0 .. code.last loop
          --      Put (Character'Val (code.oct (p) + Character'Pos ('0')));
          --    end loop;
          --    New_Line;
          --  end if;
          if code.last = prog.last
            and then Equal_Slice (code, prog, d + 1, code.last)
          then
            if x < best then
              --  Put_Line ("*** " & x'Image & "; d =" & d'Image & "; code.last =" & code.last'Image);
              best := x;
            end if;
          elsif code.last < prog.last then
            Match (x, d + 1);
          end if;
        end if;
      end loop;
    end Match;

  begin
    Convert (prog_s, prog);
    Match (0, 0);
    r (part_2) := Trim_Left (+best'Image);
  end Do_Part_2;

  compiler_test_mode : constant Boolean := Argument_Count >= 1;
  T0 : constant Time := Clock;

  kind : constant Data_Type := mini;

  prog_s : VString;

begin
  case kind is

    when mini =>
      --  Register A: 729
      --  Register B: 0
      --  Register C: 0
      --
      --  Program: 0,1,5,4,3,0

      Do_Part_1 (729, +"015430");

      if not compiler_test_mode then
        --  Register A: 2024
        --  Register B: 0
        --  Register C: 0
        --
        --  Program: 0,3,5,4,3,0

        Do_Part_2 (+"035430", False);

      end if;

    when input =>
      --  Register A: 65804993
      --  Register B: 0
      --  Register C: 0
      --
      --  Program: 2,4,1,1,7,5,1,4,0,3,4,5,5,5,3,0

      prog_s := +"2411751403455530";
      Do_Part_1 (65804993, prog_s);
      Do_Part_2 (prog_s, True);
      --  202941412157999
      --  202391656344111
      --  202322936867375
      --  202322936867370 --> OK.
  end case;

  if compiler_test_mode then
    if r (part_1) /= Argument (1) then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: " & r (part_1));
    Put_Line (+"Part 2: " & r (part_2));
    --  Part 1: validated by AoC: 5,1,4,0,5,1,0,2,6 (mini: 4,6,3,5,6,3,5,2,1,0)
    --  Part 2: validated by AoC: 202322936867370   (mini: 117440)
  end if;
end AoC_2024_17;
