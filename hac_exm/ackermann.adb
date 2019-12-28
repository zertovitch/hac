--  https://en.wikipedia.org/wiki/Ackermann_function
--  https://rosettacode.org/wiki/Ackermann_function#Ada

with HAC_Pack; use HAC_Pack;

procedure Ackermann is

   --  !! Note: we should use Natural here (not yet defined by HAC).

   function A (M, N : Integer) return Integer is
   begin
      if M = 0 then
         return N + 1;
      elsif N = 0 then
         return A (M - 1, 1);
      else
         return A (M - 1, A (M, N - 1));
      end if;
   end A;

begin
   Put_Line ("Ackermann function");
   --  with M >= 4, numbers and recursions become HUGE
   --  - see table of values in the Wikipedia page !
   for M in 0 .. 3 loop
      --  !! HAC borks with N > 4, probably a stack
      --     overflow due to of recursion.
      for N in 0 .. 4 loop
         Put (A (M, N));
      end loop;
      New_Line;
   end loop;
end Ackermann;
