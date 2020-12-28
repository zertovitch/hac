--  Pseudo-Random Generation: Rand (discrete) and Rnd (continuous)

with HAC_Pack;  use HAC_Pack;

procedure Random is

  procedure Discrete (iterations : Integer) is
    faces : constant := 6;
    histogram : array (1 .. faces) of Integer;
    r: Integer;
  begin
    for f in 1 .. faces loop
      histogram (f) := 0;
    end loop;
    for it in 1 .. iterations loop
      r := 1 + Rand (5);
      histogram (r) := histogram (r) + 1;
    end loop;
    for f in 1 .. faces loop
      Put (Integer (1000.0 * Real (histogram (f)) / Real (iterations)));
      Put (" %0 "); 
      Put_Line (histogram (f));
    end loop;
  end Discrete;
  
  procedure Continuous (iterations : Integer) is
    in_disc : Integer := 0;
  begin
    for it in 1 .. iterations loop
      if Rnd ** 2 + Rnd ** 2 <= 1.0 then
        in_disc := in_disc + 1;
      end if;
    end loop;
    Put ("Monte-Carlo estimation of pi: ");
    Put_Line (4.0 * Real (in_disc) / Real (iterations));
  end Continuous;
  
  it : Integer;
  
begin
  for dec in 3 .. 6 loop
    Put_Line( +"======= Iterations: 10 ** " & dec );
    it := 10 ** dec;
    Discrete (it);
    Continuous (it);
  end loop;
end Random;
  