--  https://rosettacode.org/wiki/Anti-primes#Ada
--
--  The anti-primes (or highly composite numbers, sequence A002182 in
--  the OEIS) are the natural numbers with more factors than any
--  smaller than itself.

with HAT; use HAT;

procedure Anti_Primes is

  function Count_Divisors (n : Integer) return Integer is
    count : Integer := 1;
  begin
    for i in 1 .. n / 2 loop
      if n mod i = 0 then
        count := count + 1;
      end if;
    end loop;
    return count;
  end Count_Divisors;

  stop : constant := 15;

  results, div_count : array (1 .. stop) of Integer;

  procedure Search is
    candidate    : Integer := 1;
    divisors     : Integer;
    max_divisors : Integer := 0;
  begin
    for i in 1 .. stop loop
      loop
        divisors := Count_Divisors (candidate);
        if max_divisors < divisors then
           results (i)   := candidate;
           div_count (i) := divisors;
           max_divisors  := divisors;
           exit;
        end if;
        candidate := candidate + 1;
      end loop;
    end loop;
  end Search;

begin
  Put_Line (+"The first " & stop & " anti-primes are:");
  Search;
  --
  for i in 1 .. stop loop
    Put (results (i), 5);
    Put (" has");
    Put (div_count (i), 3);
    Put_Line (" divisors.");
  end loop;
end Anti_Primes;
