--  Output should be empty if the compiler is correct.

with HAT;
with Testing_Utilities;

procedure Loops is
  use HAT, Testing_Utilities;
begin
  Ident_For :
  for i in 1 .. 10 loop
    exit when i = 5;
    Assert (i < 5, +"Compiler bug [A (missed exit]");
  end loop Ident_For;
  --
  for i in reverse 1 .. 10 loop
    exit when i = 5;
    Assert (i > 5, +"Compiler bug [B (missed exit]");
  end loop;
  --
  --  for b in Boolean loop put_line (b); end loop;
end Loops;
