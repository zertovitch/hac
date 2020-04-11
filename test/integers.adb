--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

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

begin
  v.x1 := 1;
  v.x2 := 3;
  x3 := 5;
  v.x3 := 6;
  if x3 /= 5 then
    Put_Line ("Compiler bug [A]");
  end if;
  x1 := v.x1;
  if x1 /= 1 then
    Put_Line ("Compiler bug [B]");
  end if;
  x3 := v.x3;
  if x3 /= 6 then
    Put_Line ("Compiler bug [C]");
  end if;
  --
  for i in 1 .. 10 loop
    v.x1 := Fibonacci (i);
    v.x2 := Fibonacci (i + 1);
    v.x3 := Fibonacci (i + 2);
    if not (v.x1 - v.x3 + v.x2 = 0) then
      Put_Line ("Compiler bug [D]");
    end if;
  end loop;
  --
  if 12_000 /= 12e003 then
    Put_Line ("Compiler bug [E]");
  end if;
  --
end Integers;
