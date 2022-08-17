--  https://en.wikipedia.org/wiki/Ackermann_function
--  https://rosettacode.org/wiki/Ackermann_function#Ada

with HAT;

procedure Ackermann is

  function A (M, N : Natural) return Positive is
  begin
    if M = 0 then
      return N + 1;
    elsif N = 0 then
      return A (M - 1, 1);
    else
      return A (M - 1, A (M, N - 1));
    end if;
  end A;

  use HAT;

begin
  Put_Line ("Ackermann function");
  --  With M >= 4, numbers and recursions become HUGE.
  --  See table of values in the Wikipedia page!
  for M in 0 .. 3 loop
    for N in 0 .. 6 loop
      Put (A (M, N), 6);
    end loop;
    New_Line;
  end loop;
end Ackermann;
