with HAC_Pack; use HAC_Pack;

procedure Var_Init is

  --  Test "atomic" (<- in the PCode sense) variables, explicit
  --  initialization.
  procedure Atomic_Explicit is
    a, b, c : Integer := 777;
    d, e, f : constant Integer := 111;
  begin
    if a - 666 /= d or b / 7 /= e or c + f /= 888 then
      Put_Line ("Compiler bug [Atomic_Explicit]");
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
    a := 3;
    b := 4;
    c := 5;
  end Atomic_Explicit;

  --  Test "atomic" (<- in the PCode sense) variables, implicit
  --  initialization.
  procedure Atomic_Implicit is
    a, b, c : VString;
  begin
    a := a & b & "abc" & c;
    if a /= "abc" then
      Put_Line ("Compiler bug [Atomic_Implicit]");
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
    b := +"gruik";
    c := +"grrrr";
  end Atomic_Implicit;

  tries : constant := 3;

begin
  for trie in 1 .. tries loop
    Atomic_Explicit;
  end loop;
  for trie in 1 .. tries loop
    Atomic_Implicit;
  end loop;
  for trie in 1 .. tries loop
    Atomic_Explicit;
    Atomic_Implicit;
  end loop;
end Var_Init;
