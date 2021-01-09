--  Output should be empty if the compiler is correct.

with HAL; use HAL;

procedure Integers is

  x1 : Integer;
  x2 : Integer;
  x3 : Integer;

  function Fibonacci (P: Integer) return Integer is
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
      pt := patch_trap;
      if -pt /= 1 then
        Put (pt); Put_Line ("  Compiler bug [Patch]");
        Set_Exit_Status (1);  --  Compiler test failed.
      end if;
    end loop;
  end Test_Patching;

begin
  v.x1 := 1;
  v.x2 := 3;
  x3 := 5;
  v.x3 := 6;
  if x3 /= 5 then
    Put_Line ("Compiler bug [A]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  x1 := v.x1;
  if x1 /= 1 then
    Put_Line ("Compiler bug [B]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  x3 := v.x3;
  if x3 /= 6 then
    Put_Line ("Compiler bug [C]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  --
  for i in 1 .. 10 loop
    v.x1 := Fibonacci (i);
    v.x2 := Fibonacci (i + 1);
    v.x3 := Fibonacci (i + 2);
    if not (v.x1 - v.x3 + v.x2 = 0) then
      Put_Line ("Compiler bug [D]");
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  end loop;
  --
  if 12_000 /= 12e003 then
    Put_Line ("Compiler bug [E]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  --
  Test_Patching;
end Integers;
