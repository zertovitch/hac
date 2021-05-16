--  Output should be empty if the compiler is correct.

with HAL;

procedure Init_Var is

  type Rec_1 is record
    i : Integer;
    r : Real;
    j : Integer;
  end record;
  
  subtype Arr_Range is Integer range 7 .. 13;
  
  type Arr_1 is array (Arr_Range, Arr_Range) of Rec_1;
  
  type Rec_2 is record
    i : Integer;
    a : Arr_1;
  end record;
  
  type Arr_2 is array (Boolean) of Rec_2;

  procedure Init_Nest_Arr (u : Arr_2; v : out Arr_2) is
    w1, w2 : Arr_2 := u;  --  The actual test is here :-) .
  begin
    for r1 in Arr_Range loop
      for r2 in Arr_Range loop
        v (True).a (r1, r2).i := 123 * w1 (True).a (r1, r2).j;
      end loop;
    end loop;
  end;    

  dummy_1 : Real := 101010.0;

  procedure Init_Nest_Rec (u : Rec_2; v : out Rec_2) is
    w1, w2 : Rec_2 := u;  --  The actual test is here :-) .
  begin
    for r1 in Arr_Range loop
      for r2 in Arr_Range loop
        v.a (r1, r2).i := 456 * w1.a (r1, r2).j;
      end loop;
    end loop;
  end;    

  dummy_2 : Integer := 101010 * 1 *2 * 3;

  x, y, z : Arr_2;  --  Two complicated data
  
  r : Rec_2;

  use HAL;

begin
  for b in Boolean loop
    x (b).i := 1 + Rand (10);
    y (b).i := 0;
    --
    for r1 in Arr_Range loop
      for r2 in Arr_Range loop
        x (b).a (r1, r2).i :=  r1;
        x (b).a (r1, r2).j :=  r1 * r2;
      end loop;
    end loop;
    y (b).a := x (b).a;
  end loop;
  --
  Init_Nest_Arr (y, z);
  if z (True).a (9, 11).i /= 9 * 11 * 123 then
    Put_Line ("Compiler bug [Arr]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  Init_Nest_Rec (y (True), r);
  if r.a (7, 13).i /= 7 * 13 * 456 then
    Put_Line ("Compiler bug [Rec]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
end Init_Var;
