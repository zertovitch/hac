WITH HAC_Pack; USE HAC_Pack;

PROCEDURE   Test2   IS

    Ten: CONSTANT INTEGER := 10;     -- Integer
    CCh: CONSTANT Character := 'A';          -- Character

    mmm: integer := 99;

Task T1;
Task T2 is
  entry Hereza_num(v1: integer);
  entry Gimmea_num(v2: out integer);
end T2;

FUNCTION  Add(x, y: integer) RETURN integer IS
  value: integer;
  BEGIN
    value :=  ( x + y );
    RETURN value;
  END Add;

Task body T1 is
  iii: integer;
  begin
    for i in 1..10 loop
      PUT("T1  ");
    end loop;
    new_line;
    iii := 42;
    Put("iii = ");
    Put_Line(iii);
    new_line;
    T2.Hereza_num(iii);
  end;

Task body T2 is
  jjj: integer;
  begin
    for i in 1..10 loop
      put("T2  ");
    end loop;
    new_line;
    jjj := 0;
    Put("jjj = ");
    Put_Line(jjj);
    Accept Hereza_num(v1: integer) do
      jjj := v1;
    end Hereza_num;
    Put("jjj = ");
    Put_Line(jjj);
    Accept Gimmea_num(v2: out integer) do
      V2 := jjj;
    end Gimmea_num;
  end;

BEGIN
  Put("mmm = ");
  Put(mmm);
  New_Line;
  for i in 1..5 Loop
    put("Main");
  end loop;
  T2.Gimmea_num(mmm);
  Put("mmm = ");
  Put(mmm);
  New_Line;
  for i in 1..5 loop
    put("<==>");
  end loop;
END Test2;
