--  https://www.mathekalender.de/wp/de/kalender/aufgaben/2022-09-de/

with HAT;

procedure Mathe_Plus_2022_09 is

  type Gift is (warm_socks, candle, bobble_hat, flute, woolen_sweater);

  size, joy : array (Gift) of Positive;

  type Child is (Nasti, Manu, Jona, Uli);

  list : array (Child, 1 .. 5) of Gift;

  total_size : Natural;
  total_joy : array (Child) of Natural;

  use HAT;

begin
  size (warm_socks)     := 2;
  size (candle)         := 4;
  size (bobble_hat)     := 6;
  size (flute)          := 24;
  size (woolen_sweater) := 16;

  joy (warm_socks)     := 4;
  joy (candle)         := 5;
  joy (bobble_hat)     := 8;
  joy (flute)          := 20;
  joy (woolen_sweater) := 10;

  list (Nasti, 1) := flute;
  list (Nasti, 2) := woolen_sweater;
  list (Nasti, 3) := bobble_hat;
  list (Nasti, 4) := candle;
  list (Nasti, 5) := warm_socks;

  list (Manu, 1) := warm_socks;
  list (Manu, 2) := candle;
  list (Manu, 3) := bobble_hat;
  list (Manu, 4) := woolen_sweater;
  list (Manu, 5) := flute;

  list (Jona, 1) := warm_socks;
  list (Jona, 2) := bobble_hat;
  list (Jona, 3) := candle;
  list (Jona, 4) := flute;
  list (Jona, 5) := woolen_sweater;

  list (Uli, 1) := flute;
  list (Uli, 2) := bobble_hat;
  list (Uli, 3) := candle;
  list (Uli, 4) := warm_socks;
  list (Uli, 5) := woolen_sweater;

  for c in Child loop
    Put (Child'Image (c) & "     ");
  end loop;
  New_Line;

  for bag_size in 2 .. 52 loop
    for c in Child loop
      total_size    := 0;
      total_joy (c) := 0;
      for i in 1 .. 5 loop
        if total_size + size (list (c, i)) <= bag_size then
          --  In the bag!
          total_size    := total_size    + size (list (c, i));
          total_joy (c) := total_joy (c) + joy  (list (c, i));
        end if;
      end loop;
      Put (total_joy (c), 8);
    end loop;
    Put ("   " & Boolean'Image (total_joy (Jona) >= total_joy (Manu)));
    New_Line;
  end loop;
end Mathe_Plus_2022_09;
