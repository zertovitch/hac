--  Output should be empty if the compiler is correct.

with HAC_Pack; use HAC_Pack;

procedure Loops is
begin
  Ident_For:
  for i in 1 .. 10 loop
    exit when i = 5;
    if i >= 5 then
      Put_Line ("Compiler bug [A (missed exit]");
    end if;
  end loop Ident_For;
  --
  for i in reverse 1 .. 10 loop
    exit when i = 5;
    if i <= 5 then
      Put_Line ("Compiler bug [B (missed exit]");
    end if;
  end loop;
end Loops;
