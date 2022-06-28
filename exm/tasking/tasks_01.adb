with HAL; use HAL;

procedure Tasks_01 is

  task T1;

  task body T1 is
  begin
    for i in 1 .. 10 loop
      Put_Line ("I am T1");
      delay 0.01;
    end loop;
  end T1;

begin
  for i in 1 .. 10 loop
    Put_Line ("I am the main procedure");
    delay 0.01;
  end loop;
end Tasks_01;
