with HAC_Pack; use HAC_Pack;

procedure Hello is
begin
  Put("Hello");
  Put_Line(" world!");
  for i in 1 .. 10 loop
    Put("Spaceman is speaking...");
    Put(i);  --  !! width parameter ??
    Put(Sqr(i));  --  !! ** operator ??
    New_Line;
  end loop;
end;
