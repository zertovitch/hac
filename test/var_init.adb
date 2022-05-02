with HAL;
with Testing_Utilities;

procedure Var_Init is
  use HAL, Testing_Utilities;

  --  Test "atomic" (<- in the PCode sense) variables, explicit
  --  initialization.
  procedure Atomic_Explicit is
    a, b, c : Integer := 777;
    d, e, f : constant Integer := 111;
  begin
    Assert (a - 666 = d and b / 7 = e and c + f = 888, +"Compiler bug [Atomic_Explicit]");
    a := 3;
    b := 4;
    c := 5;
  end Atomic_Explicit;

  --  Test "atomic" (<- in the PCode sense) variables, implicit
  --  initialization.
  procedure Atomic_Implicit is
    a, b, c : VString;  --  Initialized as empty VString's (= Unbounded_String's).
  begin
    a := a & b & "abc" & c;
    Assert (a = "abc", +"Compiler bug [Atomic_Implicit]");
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
