--  We check numerical recursive functions (and
--  also a bit the correctness of array operations).

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

  procedure Ackarray is
    M_Max : constant := 3;
    N_Max : constant := 4;
    type Storage is array (0 .. M_Max, 0 .. N_Max) of Integer;
    --
    Noise_1 : Integer := 11111;
    A : Storage;
    Noise_2 : Integer;
    B : Storage;
    Noise_3 : Integer;    
  begin
    for M in 0 .. M_Max loop
      for N in reverse 0 .. N_Max loop
        A (M, N) := Ackermann (M, N);
      end loop;
    end loop;
    --
    Noise_2 := 22222;
    B := A;
    Noise_3 := 33333;
    --
    for N in reverse 0 .. N_Max loop
      for M in 0 .. M_Max loop
        if B (M, N) /= Ackermann (M, N) then
          Put_Line ("Compiler bug [Array]");
        end if;
      end loop;
    end loop;
    --
    if Noise_1 + Noise_2 /= Noise_3 then
      Put_Line ("Compiler bug [Stack]");
    end if;
  end Ackarray;

begin
  if Fibonacci (22) /= 17_711 then
    Put_Line ("Compiler bug [Fibonacci]");
  end if;
  if Ackermann (3, 4) /= 125 then
    Put_Line ("Compiler bug [Ackermann]");
  end if;
  Ackarray;
end Recursion;
