with HAC_Pack; use HAC_Pack;

procedure   Test2   is

  Ten: constant INTEGER := 10;     -- Integer
  CCh: constant Character := 'A';          -- Character

  Mmm: Integer := 99;

  task T1;
  task T2 is
    entry Hereza_Num(V1: Integer);
    entry Gimmea_Num(V2: out Integer);
  end T2;

  function  Add(X, Y: Integer) return Integer is
    Value: Integer;
  begin
    Value :=  ( X + Y );
    return Value;
  end Add;

  task body T1 is
    Iii: Integer;
  begin
    for I in 1..10 loop
      PUT("Task T1  ");
    end loop;
    New_Line;
    Iii := 42;
    Put("iii = ");
    Put_Line(Iii);
    New_Line;
    T2.Hereza_Num(Iii);
  end T1;

  task body T2 is
    Jjj: Integer;
  begin
    for I in 1..10 loop
      Put("Task T2  ");
    end loop;
    New_Line;
    Jjj := 0;
    Put("jjj = ");
    Put_Line(Jjj);
    accept Hereza_Num(V1: Integer) do
      Jjj := V1;
    end Hereza_Num;
    Put("jjj = ");
    Put_Line(Jjj);
    accept Gimmea_Num(V2: out Integer) do
      V2 := Jjj;
    end Gimmea_Num;
  end T2;

begin
  Put("Test with Tasks.");
  New_Line;
  Put("[point 1] mmm = ");
  Put(Mmm);
  New_Line;
  for I in 1..5 loop
    Put("Main");
  end loop;
  New_Line;
  T2.Gimmea_Num(Mmm);
  Put("[point 2] mmm = ");
  Put(Mmm);
  New_Line;
  for I in 1..5 loop
    Put("<==>");
  end loop;
  New_Line;
  Put("Done.");
end Test2;
