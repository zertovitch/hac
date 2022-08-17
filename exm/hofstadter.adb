--  Hofstadter Female and Male sequences
--  Code taken from:
--  http://rosettacode.org/wiki/Mutual_recursion#Ada
--

with HAT;

procedure Hofstadter is

  function M (N : Integer) return Integer;

  function F (N : Integer) return Integer is
  begin
    if N = 0 then
      return 1;
    else
      return N - M (F (N - 1));
    end if;
  end F;

  function M (N : Integer) return Integer is
  begin
    if N = 0 then
      return 0;
    else
      return N - F (M (N - 1));
    end if;
  end M;

  --  F(n) is not equal to M(n) if and only if n+1 is a Fibonacci number.
  --  https://oeis.org/A005378

  function Fibonacci (N : Integer) return Integer is
  begin
    if N = 0 then
      return 0;
    elsif N = 1 then
      return 1;
    else
      return Fibonacci (N - 1) + Fibonacci (N - 2);
    end if;
  end Fibonacci;

  n_max : constant := 35;
  dig_it : constant := 3;

  is_fibo : array (0 .. n_max) of Boolean;
  fn : Integer;

  use HAT;

begin
  Put_Line ("Hofstadter Female and Male sequences (mutual recursion)");
  for n in 0 .. n_max - 1 loop
    is_fibo (n) := False;
  end loop;
  for n in 0 .. n_max - 1 loop
    fn := Fibonacci (n);
    exit when fn > is_fibo'Last;
    is_fibo (fn) := True;
  end loop;
  Put ("n =     ");
  for n in 0 .. n_max - 1 loop Put (n, dig_it); end loop;
  New_Line;
  Put_Line ("        " & n_max * dig_it * '_');
  Put_Line ("Is n+1  ");
  Put ("Fibo ?  ");
  for n in 0 .. n_max - 1 loop
    Put ((dig_it - 1) * ' ');
    if is_fibo (n + 1) then
      Put ('Y');
    else
      Put (' ');
    end if;
  end loop;
  New_Line;
  Put ("F:      ");
  for n in 0 .. n_max - 1 loop Put (F (n), dig_it); end loop;
  New_Line;
  Put ("M:      ");
  for n in 0 .. n_max - 1 loop Put (M (n), dig_it); end loop;
  New_Line;
  Put ("F = M   ");
  for n in 0 .. n_max - 1 loop
    Put ((dig_it - 1) * ' ');
    if F (n) /= M (n) then
      Put ('N');
    else
      Put (' ');
    end if;
  end loop;
  New_Line;
end Hofstadter;
