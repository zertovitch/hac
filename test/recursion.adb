--  We check numerical recursive functions (and also a bit the
--  correctness of array operations, and nested subprograms too).

with HAT;
with Testing_Utilities;

procedure Recursion is
  use HAT, Testing_Utilities;

  function Fibonacci (P : Natural) return Positive is
  begin
    if P <= 2 then
      return 1;
    else
      return Fibonacci (P - 1) + Fibonacci (P - 2);
    end if;
  end Fibonacci;

  function Ackermann (M, N : Natural) return Positive is
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
    Noise_1 : constant Integer := 11111;
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
        Assert (B (M, N) = Ackermann (M, N), +"Compiler bug [Recursion, Ackermannm, Array]");
      end loop;
    end loop;
    --
    Assert (Noise_1 + Noise_2 = Noise_3, +"Compiler bug [Stack]");
  end Ackarray;

  procedure Nesting_Tests is
    --  We compute in an horribly complicated way the value: 2 ** Level - 1.
    --  This is for testing recursion *and* nested subprograms together.

    Max_L : constant := 20;

    type Usine_a_Gaz is record
      ant, bat : Real;
      N        : Integer;
      cat, dog : Boolean;
    end record;

    procedure Nesting_Test_P is
      --  Outer calls inner and vice-versa.
      procedure Add_1_and_shift (U : in out Usine_a_Gaz; Level : Integer) is
        procedure Shift_and_add_1 (U : in out Usine_a_Gaz) is
        begin
          if Level > 1 then
            U.N := U.N * 2;
            Add_1_and_shift (U, Level - 1);
          end if;
        end Shift_and_add_1;
      begin
        U.N := U.N + 1;
        Shift_and_add_1 (U);
      end Add_1_and_shift;
      R : Usine_a_Gaz;
    begin
      for L in 1 .. Max_L loop
        R.N := 0;
        Add_1_and_shift (R, L);
        Assert (R.N = 2 ** L - 1, +"Compiler bug [Recursion, Nesting_Test_P]");
      end loop;
    end Nesting_Test_P;

    procedure Nesting_Test_F is
      --  Outer calls inner and vice-versa.
      function Add_1_and_shift (N : Integer; Level : Integer) return Integer is
        function Shift_and_add_1 (N : Integer) return Integer is
        begin
          if Level > 1 then
            return Add_1_and_shift (N * 2, Level - 1);
          end if;
          return N;
        end Shift_and_add_1;
      begin
        return Shift_and_add_1 (N + 1);
      end Add_1_and_shift;
    begin
      for L in 1 .. Max_L loop
        Assert (Add_1_and_shift (0, L) = 2 ** L - 1, +"Compiler bug [Recursion, Nesting_Test_F]");
      end loop;
    end Nesting_Test_F;

  begin
    Nesting_Test_P;
    Nesting_Test_F;
  end Nesting_Tests;

begin
  Assert (Fibonacci (22) = 17_711, +"Compiler bug [Recursion, Fibonacci]");
  Assert (Ackermann (3, 4) = 125,  +"Compiler bug [Recursion, Ackermann]");
  Ackarray;
  Nesting_Tests;
end Recursion;
