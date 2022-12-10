--  https://www.mathekalender.de/wp/de/kalender/aufgaben/2022-02-de/
--
--  Brute-Force-Methode zum Raestsel "Bogenmatik"
--
--  a*5 = Punkte pro Treffer im weissen Ring.
--  b*5 = Punkte pro Treffer im violetten Ring.
--  c*5 = Punkte pro Treffer im gelben Ring.
--  d*5 = Punkte pro Treffer im Zentrum.

with HAT;

procedure Mathe_Plus_2022_02 is
  lim : constant := 22;
  use HAT;
begin
  for a in 1 .. lim loop
    for b in a + 1 .. lim + 1 loop
      for c in b + 1 .. lim + 2 loop
        for d in c + 1 .. lim + 3 loop
          if    3 * a +     b +             d = 25  --  125 Punkte gesamt.
            and         2 * b +     c + 2 * d = 46  --  230 Punkte gesamt.
            and   a +       b + 2 * c +     d = 37  --  185 Punkte gesamt.
          then
            HAT.Put_Line
              (+"Moeglichkeit: " &
               a * 5 & ", " & b * 5 & ", " & c * 5 & ", " & d * 5);
          end if;
        end loop;
      end loop;
    end loop;
  end loop;
end Mathe_Plus_2022_02;
