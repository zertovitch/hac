--  https://rosettacode.org/wiki/Anti-primes#Ada

with HAC_Pack; use HAC_Pack;

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
 
  stop : constant := 10;
 
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
   Search;
   --
   Put_Line ("The first anti-primes are:");
   for i in 1 .. stop loop
      Put (results (i));
      Put(" has");
      Put (div_count (i));
      Put(" divisors.");
      New_Line;
   end loop;
   New_Line;
end Anti_Primes;