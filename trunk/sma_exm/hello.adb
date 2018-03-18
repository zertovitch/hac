with HAC_Pack; use HAC_Pack;

procedure Hello is
begin
  Put("Hello");
  Put_Line(" world!");
  for i in 1 .. 10 loop
    Put("Spaceman is speaking...");
    Put(i);  --  !! width parameter ??
    Put(i ** 2);
    Put(2 ** i);
    New_Line;
  end loop;
end;
