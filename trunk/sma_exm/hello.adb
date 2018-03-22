with HAC_Pack; use HAC_Pack;

procedure Hello is
  f : Float := 0.0;
begin
  Put("Hello");
  Put_Line(" world!");
  for i in 1 .. 10 loop
    Put("Spaceman is speaking...");
    Put(i);  --  !! width parameter ??
    Put(i ** 2);
    Put(2 ** i);
    Put(2.0 ** i);
    f := f + 1.0;
    Put(2.0 ** f);
    New_Line;
  end loop;
end;
