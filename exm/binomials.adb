with HAT;

procedure Binomials is

  function Factorial (n : Natural) return Positive is
  begin
    if n = 0 then
      return 1;
    else
      return n * Factorial (n - 1);
    end if;
  end Factorial;

  --  Returns the binomial coefficient  /n\
  --                                    \k/

  function Binomial (n, k : Natural) return Positive is
  begin
    return Factorial (n) / (Factorial (k) * Factorial (n - k));
  end Binomial;

  use HAT;

begin
  for n in 1 .. 10 loop
    Put ("n = "); Put_Line (n, 0);
    Put_Line ("k= binomial (n,k)=");
    for k in 1 .. n - 1 loop
      Put (k, 0); Put (": "); Put_Line (Binomial (n, k), 0);
    end loop;
    New_Line;
  end loop;
end Binomials;
