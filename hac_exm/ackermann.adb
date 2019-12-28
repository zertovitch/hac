--  https://en.wikipedia.org/wiki/Ackermann_function
--  https://rosettacode.org/wiki/Ackermann_function#Ada

with HAC_Pack; use HAC_Pack;
 
procedure Test_Ackermann is

   --  !! Note: we should use Natural here (not yet defined by HAC).

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
   Put_Line ("Ackermann function");
   for M in 0 .. 3 loop
      for N in 0 .. 4 loop  --  !! HAC borks with N > 5 (recursion).
         Put (Ackermann (M, N));
      end loop;
      New_Line;
   end loop;
end Test_Ackermann;
