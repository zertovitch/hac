--  Puzzle presented by John Barnes in the restaurant "De Abt" in Ghent,
--  the 15th of June 2022, during the Ada-Europe 2022 conference
--            http://www.ada-europe.org/conference2022/
--
--  The question is (from memory):
--
--       "Find the integer (there is only one) with all decimal digits appearing
--        once and only once, for which the number formed by the first
--        two digits can be divided by two, the number formed by the
--        first three digits can be divided by three, and so on."
--
--  Solution by J-P Rosen, adapted to the HAC subset and added to HAC's regression suite.
--  The original solution is reproduced as a comment at the bottom of this procedure.

with HAT;
with Interfaces;

procedure Barnes is
   use HAT, Interfaces;

   subtype My_Int is Interfaces.Integer_64 range 0 .. 1e11;
   subtype Digit_Range is My_Int range 1 .. 10;
   type Digit_String is array (Digit_Range) of My_Int;

   subtype Coeff_Range is My_Int range 0 .. 9;

   Coeff : array (Coeff_Range) of My_Int;

   compiler_regression_test_mode : constant Boolean := Argument_Count > 0;

   procedure Put_Solution (S : Digit_String) is
      Chars : constant array (Coeff_Range) of Character := "0123456789";
      Res : My_Int := 0;
   begin
      if compiler_regression_test_mode then
         for I in S'Range loop
            Res := Res * 10 + Character'Pos (Chars (S (I))) - Character'Pos ('0');
         end loop;
         if Res /= My_Int'Value (To_String (Argument (1))) then
            Put_Line ("   ----> Compiler test failed.");
            Set_Exit_Status (1);
         end if;
      else
         Put ("Solution = ");
         for I in S'Range loop
            Put (Chars (S (I)));
         end loop;
         New_Line;
      end if;
   end Put_Solution;

   function Is_Possible (S : Digit_String; Last : My_Int) return Boolean is
      Seen : array (Coeff_Range) of Boolean;
      Accu : My_Int := 0;
   begin
      for K in Seen'Range loop
        Seen (K) := False;
      end loop;
      for I in S'First .. Last loop
         if Seen (S (I)) then
            return False;
         end if;
         Seen (S (I)) := True;
         Accu := Accu + S (I) * Coeff (Last - I);
      end loop;
      return Accu mod Last = 0;
   end Is_Possible;

   subtype My_Positive_Digit is My_Int range 1 .. 9;
   subtype My_One_To_Four_Digit is My_Int range 1 .. 4;

   Candidate : Digit_String;

   procedure Try_Combinations is
      D5 : constant := 5;
   begin
      Candidate (5) := D5;
      --  Small optimizations:
      --  Last digit is 0 (dividable by 10) => not other digit is 0
      --  Fifth digit is 5 (dividable by 5 and not a 0)
      --  Second digit is even => 2, 4, 6, 8
      for D1 in My_Positive_Digit loop
         Candidate (1) := D1;
         for D2 in My_One_To_Four_Digit loop
            Candidate (2) := 2 * D2;
            if Is_Possible (Candidate, 2) then
               for D3 in My_Positive_Digit loop
                  Candidate (3) := D3;
                  if Is_Possible (Candidate, 3) then
                     for D4 in My_Positive_Digit loop
                        Candidate (4) := D4;
                        if Is_Possible (Candidate, 4) then
                           for D6 in My_Positive_Digit loop
                              Candidate (6) := D6;
                              if Is_Possible (Candidate, 6) then
                                 for D7 in My_Positive_Digit loop
                                    Candidate (7) := D7;
                                    if Is_Possible (Candidate, 7) then
                                       for D8 in My_Positive_Digit loop
                                          Candidate (8) := D8;
                                          if Is_Possible (Candidate, 8) then
                                             for D9 in My_Positive_Digit loop
                                                Candidate (9) := D9;
                                                if Is_Possible (Candidate, 9) then
                                                   Candidate (10) := 0;
                                                   Put_Solution (Candidate);
                                                   return;
                                                end if;
                                             end loop;
                                          end if;
                                       end loop;
                                    end if;
                                 end loop;
                              end if;
                           end loop;
                        end if;
                     end loop;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;
   end Try_Combinations;

begin
   if not compiler_regression_test_mode then
      Put_Line ("Find the integer (there is only one) with all decimal digits appearing");
      Put_Line ("once and only once, for which the number formed by the first");
      Put_Line ("two digits can be divided by two, the number formed by the");
      Put_Line ("first three digits can be divided by three, and so on.");
      New_Line;
   end if;
   --
   Coeff (0) := 1;
   for I in My_Positive_Digit loop
      Coeff (I) := Coeff (I - 1) * 10;
   end loop;
   --
   Try_Combinations;
end Barnes;

------------------------------------------------
--  Original "full Ada" program by J-P Rosen  --
------------------------------------------------
--
--
--  with Ada.Text_IO;
--  procedure Barnes is
--     use Ada.Text_Io;
--
--     type My_Int is range 0 .. 10**11;
--     Terminated : exception;
--     D5         : constant My_Int := 5;
--     type Digit_String is array (My_Int range <>) of My_Int;
--     Coeff      : array (My_Int range 0 .. 9) of My_Int;
--
--     procedure Put (S : Digit_String) is
--        Chars : constant array (My_Int range 0 .. 9) of Character := "0123456789";
--     begin
--        for I in S'Range loop
--           Put (Chars (S (I)));
--        end loop;
--     end Put;
--
--     function Is_Possible (S : Digit_String) return Boolean is
--        Seen : array (My_Int range 0..9) of Boolean := (others => False);
--        Accu : My_Int := 0;
--     begin
--        for I in S'Range loop
--           if Seen (S(I)) then
--              return False;
--           end if;
--           Seen (S(I)) := True;
--           Accu := Accu + S (I) * Coeff (S'Last - I);
--        end loop;
--        return Accu mod S'Length = 0;
--     end Is_Possible;
--
--  begin
--     Coeff (0) := 1;
--     for I in My_Int range 1 .. 9 loop
--        Coeff (I) := Coeff (I - 1) * 10;
--     end loop;
--
--     -- Small optimizations:
--     -- Last digit is 0 (dividable by 10) => not other digit is 0
--     -- Fifth digit is 5 (dividable by 5 and not 0)
--     -- Second digit is even (hence not 1 or 9)
--     for D1 in My_Int range 1 .. 9 loop
--        for D2 in My_Int range 2 .. 8 loop
--           if Is_Possible ((D1, D2)) then
--              for D3 in My_Int range 1 .. 9 loop
--                 if Is_Possible ((D1, D2, D3)) then
--                    for D4 in My_Int range 1 .. 9 loop
--                       if Is_Possible ((D1, D2, D3, D4)) then
--                          for D6 in My_Int range 1 .. 9 loop
--                             if Is_Possible ((D1, D2, D3, D4, D5, D6)) then
--                                for D7 in My_Int range 1 .. 9 loop
--                                   if Is_Possible ((D1, D2, D3, D4, D5, D6, D7)) then
--                                      for D8 in My_Int range 1 .. 9 loop
--                                         if Is_Possible ((D1, D2, D3, D4, D5, D6, D7, D8)) then
--                                            for D9 in My_Int range 1 .. 9 loop
--                                               if Is_Possible ((D1, D2, D3, D4, D5, D6, D7, D8, D9)) then
--                                                  Put ("Solution = ");
--                                                  Put (Digit_String'(D1, D2, D3, D4, D5, D6, D7, D8, D9, 0));
--                                                  New_Line;
--                                                  raise Terminated;
--                                               end if;
--                                            end loop;
--                                         end if;
--                                      end loop;
--                                   end if;
--                                end loop;
--                             end if;
--                          end loop;
--                       end if;
--                    end loop;
--                 end if;
--              end loop;
--           end if;
--        end loop;
--     end loop;
--  exception
--     when Terminated =>
--        null;
--  end Barnes;
