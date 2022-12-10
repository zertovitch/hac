--  Output should be empty if the compiler is correct.

with HAT;
with Testing_Utilities;

procedure Integers is
  use HAT, Testing_Utilities;

  x1 : Integer;
  x2 : Integer;
  x3 : Integer;

  function Fibonacci (P : Integer) return Integer is
  begin
    if P <= 2 then
      return 1;
    else
      return Fibonacci (P - 1) + Fibonacci (P - 2);
    end if;
  end Fibonacci;

  type R is record
    x1 : Integer;
    x2 : Integer;
    x3 : Integer;
  end record;

  v : R;

  procedure Test_Patching is
    --  -1 is the value of dummy_address for patching IF jumps...
    patch_trap : constant := -1;
    pt : Integer;
  begin
    for i in 1 .. 1 loop
      --  SmallAda and early HAC versions patched all
      --  instructions with an operand = -1 !...
      pt := patch_trap;
      if -pt /= 1 then
        Put (pt);
        Failure (+"  Compiler bug [Integers, IF Patch]");
      end if;
    end loop;
  end Test_Patching;

begin
  v.x1 := 1;
  v.x2 := 3;
  x3 := 5;
  v.x3 := 6;
  Assert (x3 = 5, +"Compiler bug [Integers, A]");
  x1 := v.x1;
  Assert (x1 = 1, +"Compiler bug [Integers, B]");
  x3 := v.x3;
  Assert (x3 = 6, +"Compiler bug [Integers, C]");
  --
  for i in 1 .. 10 loop
    v.x1 := Fibonacci (i);
    v.x2 := Fibonacci (i + 1);
    v.x3 := Fibonacci (i + 2);
    Assert (v.x1 - v.x3 + v.x2 = 0, +"Compiler bug [Integers, D]");
  end loop;
  --
  Assert (12_000 = 12e003, +"Compiler bug [Integers, E]");
  --
  Test_Patching;
  --
  Assert (HAT.Max (1, 2) = 2, +"HAT.Max [1]");
  Assert (HAT.Max (2, 1) = 2, +"HAT.Max [2]");
  --
  Assert (HAT.Min (1, 2) = 1, +"HAT.Min [1]");
  Assert (HAT.Min (2, 1) = 1, +"HAT.Min [2]");
  --
  Assert (HAT.Sgn (123)  =  1, +"HAT.Sgn [1]");
  Assert (HAT.Sgn (0)    =  0, +"HAT.Sgn [2]");
  Assert (HAT.Sgn (-123) = -1, +"HAT.Sgn [3]");
end Integers;
