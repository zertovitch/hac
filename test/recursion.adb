with HAC_Pack;  use HAC_Pack;

procedure Recursion is

    function Fibonacci (P: Integer) return Integer is
    begin
      if P <= 2 then
        return 1;
      else
        return Fibonacci (P - 1) + Fibonacci (P - 2);
      end if;
    end Fibonacci;

   function Ackermann (M, N : Integer) return Integer is
   begin
      if M = 0 then
         return N + 1;
      elsif N = 0 then
         return Ackermann (M - 1, 1);
      else
         return Ackermann (M - 1, Ackermann (M, N - 1));
      end if;
   end Ackermann;

begin
  if Fibonacci (22) /= 17711 then
    Put_Line ("Compiler bug [Fibonacci]");
  end if;
  if Ackermann (3, 4) /= 125 then
    Put_Line ("Compiler bug [Ackermann]");
  end if;
end Recursion;
