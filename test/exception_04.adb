--  We demonstrate a trace-back occurring on a non-trivial call
--  structure (recursion).

procedure Exception_04 is

  procedure Nest is  --  Copy of some code from:  recursion.adb

    Max_L : constant := 5;

    procedure NTF is
      --  Outer calls inner and vice-versa.
      function Add_n_shift (N : Integer; Level : Integer) return Integer is
        function Shift_n_add (N : Integer) return Integer is
          a : array (1 .. 3) of Integer;
          minus_4 : Integer := -4;
        begin
          if Level > 1 then
            return Add_n_shift (N * 2, Level - 1);  --  <-  Trace-back should show this line
          else
            a (minus_4) := 5;                       --  <-  *** Boom! *** : -4 is out-of-range
          end if;
          return N;
        end Shift_n_add;
      begin
        return Shift_n_add (N + 1);                 --  <-  Trace-back should show this line
      end Add_n_shift;
    begin
      for L in reverse 1 .. Max_L loop
        if Add_n_shift (0, L) /= 2 ** L - 1 then    --  <-  Trace-back should show this line
          null;
        end if;
      end loop;
    end NTF;

  begin
    NTF;                                     --  <-  Trace-back should show this line
  end Nest;

  procedure P1 is null;

  dummy : Integer;
begin
  dummy := 1234;
  Nest;        --  <-  Trace-back should show this line
  dummy := 4321;
  P1;                   --  <-  No executed due to previously raised exception.
end Exception_04;
