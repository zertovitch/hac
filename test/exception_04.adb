with HAC_Pack;  use HAC_Pack;

procedure Exception_04 is

  procedure Nest is  --  Copy of some code from:  recursion.adb

    Max_L : constant := 5;

    procedure NTF is
      --  Outer calls inner and vice-versa.
      function Add_n_shift (N: Integer; Level : Integer) return Integer is
        function Shift_n_add (N: Integer) return Integer is
          a : array (1 .. 3) of Integer;
        begin
          if Level > 1 then
            return Add_n_shift (N * 2, Level - 1);  --  <-  Trace-back should show this line
          else
            a (4) := 5;       --  <-  Boom: out-of-range
          end if;
          return N;
        end;
      begin
        return Shift_n_add (N + 1);                 --  <-  Trace-back should show this line
      end;
    begin
      for L in reverse 1 .. Max_L loop
        if Add_n_shift (0, L) /= 2 ** L - 1 then    --  <-  Trace-back should show this line
          Put_Line ("Compiler bug [NTF]");
        end if;
      end loop;
    end NTF;

  begin
    NTF;                                     --  <-  Trace-back should show this line
  end Nest;

  procedure P1 is null;

  dummy : Real;
begin
  dummy := 1.234;
  Nest;        --  <-  Trace-back should show this line
  dummy := 4.321;
  P1;                   --  <-  No executed due to previously raised exception.
end;
