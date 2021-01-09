--  Output should be empty if the compiler is correct.

with HAL; use HAL;

procedure Loops is
begin
  Ident_For:
  for i in 1 .. 10 loop
    exit when i = 5;
    if i >= 5 then
      Put_Line ("Compiler bug [A (missed exit]");
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  end loop Ident_For;
  --
  for i in reverse 1 .. 10 loop
    exit when i = 5;
    if i <= 5 then
      Put_Line ("Compiler bug [B (missed exit]");
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  end loop;
  --
  --  for b in Boolean loop put_line (b); end loop;
end Loops;
