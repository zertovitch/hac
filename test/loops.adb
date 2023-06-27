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
  L1_a :
  for i in 10001 .. 10010 loop
    L2_a :
    for j in 20002 .. 20007 loop
      exit L1_a when i = 10005;  --  exiting L2 would pass i = 10006, ...
      Assert (i < 10005, +"Compiler bug [C (missed exit]" & i'Image);
    end loop L2_a;
  end loop L1_a;
  --
  L1_b :
  for i in 30001 .. 30010 loop
    L2_b :
    loop
      L3_b :
      for j in 40002 .. 40007 loop
        L4_b :
        for k in 50004 .. 50008 loop
          exit L1_b when i = 30005;  --  exiting L2, L3, L4 would pass i = 30006, ...
          Assert (i < 30005, +"Compiler bug [D (missed exit]" & i'Image);
        end loop L4_b;
      end loop L3_b;
      exit L2_b;
    end loop L2_b;
  end loop L1_b;
end Loops;
