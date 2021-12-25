with HAL;

procedure Hello is
  use HAL;
  f : Real := 0.0;
begin
  Put ("Hello");
  Put_Line (" world! I am the program file: " & Command_Name);
  Put_Line (
        "          i     i ** 2     2 ** i      2.0 ** i      2.0 ** f");
  Put_Line (
        "-------------------------------------------------------------");
  for i in 1 .. 10 loop
    Put (i, 11);
    Put (i ** 2);
    Put (2 ** i);
    Put (2.0 ** i, 11, 2, 0);       --  Fore, Aft, Exp.
    f := f + 1.0;
    Put_Line (2.0 ** f,  5, 3, 4);  --  Fore, Aft, Exp.
  end loop;
end Hello;
