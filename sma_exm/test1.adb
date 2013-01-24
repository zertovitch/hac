WITH SMALL_SP;  USE SMALL_SP;

PROCEDURE   Test1 IS

  --This is the initial test program for the Small-Ada Compiler
  --   Small-Ada is a restircted subset of the full Ada Language
  --   The Small-Ada compiler was derived from the Co-Pascal
  --       compiler which was in turn was derived from Pascas-S

  --Declarations

       Ten: CONSTANT INTEGER := 10;     -- Integer    (*  test  *)
       Twenty: CONSTANT  := 20;
       TenpOne: CONSTANT := 10.1;        -- Float
       CA: CONSTANT  Character := 'A';          -- Character
       CE: CONSTANT  Character := 'E';          -- Character
       B1, B2: CONSTANT Boolean:= True;        -- Boolean

TYPE   Type1 IS RECORD
                 x: integer;
                 y: Float;
                END RECORD;
--TYPE       Type2 IS String(1..10);
TYPE       Type3 IS Array(1..4) of Type1;
--TYPE       Type4 IS RECORD
--                 ay: array(1..Ten) of integer;
--                END RECORD;
TYPE       Day IS (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

    i1, i2, i3:  integer := 99;
    i4: integer := -i1 + 10;
    i5, i6, i7:  integer;
    r1: Float := 1.23;
    r2: Float := TenpOne;
    c1, c2:  character := '+';
    c3: character := CA;
    b3, b4, b5:  boolean := false;
    A:  array(1..5) of integer;
    WeekDay: Day;

FUNCTION  Add(x, y: integer) RETURN integer IS
  value: integer;
  BEGIN
    value :=  ( x + y );
    RETURN value;
  END Add;

PROCEDURE IComp(x, y: integer) IS
  BEGIN
    IF (x <= y) THEN PUT("In "); PUT_LINE("Order");
                ELSE PUT_LINE("Reverse Order");  END IF;
  END IComp;

BEGIN
  --CONSTANT Check
  PUT_LINE("CONSTANT Check");
  PUT("Ten     = ");  PUT_LINE(Ten);
  PUT("Twenty  = ");  PUT_LINE(Twenty);
  PUT("TenpOne = ");  PUT_LINE(TenpOne,10,1);
  PUT("CA      = ");  PUT(CA); New_Line;
  PUT("B1, B2  = ");  PUT(B1); Put(' '); PUT_LINE(B2);
  New_LINE;

  PUT_LINE("Initialization Check");
  PUT("i1, i2, i3  "); Put(i1); Put(i2); Put(i3); New_Line;
  PUT("i4       ");    Put(i4); New_Line;
  PUT_LINE("r1, r2  "); Put(r1); Put(r2); New_Line;
  PUT_LINE("c1, c2, c3"); Put(c1); Put(c3); Put(c3); New_Line;
  PUT_LINE("b1, .., b5"); Put(b1); Put(b2); Put(b3); Put(b4); Put(b5); New_Line;
  New_LINE;

  PUT_LINE("ARITHMETIC Check");
  i1 := 13;             Put(i1); PUT_LINE(" (i1)");
  i2 := Ten;            Put(i2); PUT_LINE(" (i2)");
  i3 := i1 + i2 + 5;    Put(i3); PUT_LINE(" (i3=i1+i2+5)");
  i3 := i1 - Twenty;    Put(i3); PUT_LINE(" (i3=i1-20)");
  i3 := i2 * i1;        Put(i3); PUT_LINE(" (i3=i1*i2)");
  i3 := i1 / 5;         Put(i3); PUT_LINE(" (i3=i1 DIV 5)");
  i3 := i1 MOD 5;       Put(i3); PUT_LINE(" (i3=i1 MOD 5)");
  r1 := 20.4;           Put(r1); PUT_LINE(" (r1)");
  r2 := r1/2.0;         Put(r2); PUT_LINE(" (r1/2)");
  PUT(10.0 * tenpone); Put_Line(" (10*10.1)");
  New_LINE;

  PUT_LINE("LOOP and ARRAY Check");
  i1 := 10;
  PUT_LINE("Single FOR LOOP 1..5");
  for i IN 1 .. 5 LOOP
     a ( i ) := i * 2;
  END LOOP;
  for n IN 1 .. 5 LOOP
     PUT(n);
     Put(a(n)); Put_Line(" = n*2");
  END LOOP;
  New_LINE;
  PUT_LINE("Double FOR LOOP 1..2 A..E");
  FOR i IN 1 .. 2 LOOP
     PUT("Line: "); Put(i); Put(' ');
     for j IN CA .. CE  LOOP
        PUT(j);
     END LOOP;
     New_LINE;
  END LOOP;
  PUT_LINE("Reverse FOR LOOP 1..7");
  FOR i1 IN REVERSE 1..7 LOOP
     PUT(i1);
  END LOOP;
  PUT("  i1 = "); Put(i1);
  New_LINE;
  New_LINE;

  PUT_LINE("WHILE Check 1..5");
  i1 := 0;
  While (i1 < 5) LOOP
      i1 := i1 + 1;
      PUT(i1);
  END LOOP;
  New_LINE;

  --REPEAT Check
  New_Line;   PUT_LINE("REPEAT Check");
  LOOP
    PUT("Rpt  ");
    i1 := i1 + 1;
    Exit WHEN (i1 = Ten);
  END LOOP;
  New_Line;

  --Function and Procedure Check
  New_Line;   PUT_LINE("FUNCTION and PROCEDURE Check");
  PUT("compare 10 to 20 ");  IComp(ten, twenty);
  PUT("compare 20 to 10 ");  IComp(twenty, ten);
  PUT(ten);  PUT(5);  PUT("     ADD = "); Put(add(ten,5)); New_Line;
  New_Line;

  PUT_LINE("IF and BOOLEAN Check");
  IF ten > twenty THEN
     PUT(ten); Put('>'); Put(twenty); Put("O_o"); New_Line;
  ELSE
     PUT(ten); Put("<="); Put(twenty); New_Line;
  END IF;

  i1 := 99;
  PUT(i1); Put(" compared to 10 and 20 is :  ");
  IF i1 < 10 THEN
     PUT_LINE("the smallest");
  ELSIF i1 < 20 THEN
     PUT_LINE("in the middle");
  ELSE
     PUT_LINE("the largest");
  END IF;

  IF twenty > 10 THEN
     PUT(twenty); Put('>'); Put(ten); New_Line;
  END IF;

  IF (twenty > ten) AND True THEN
     PUT_LINE("AND is OK");
  ELSE
     PUT_LINE("AND is not OK");
  END IF;

  IF (ten > twenty) OR (Twenty > ten) THEN
     PUT_LINE ("OR is OK");
  ELSE
     PUT_LINE ("OR is not OK");
  END IF;

  IF NOT (ten > twenty) THEN
     PUT_LINE ("NOT is OK");
  ELSE
     PUT_LINE ("NOT is not OK");
  END IF;

  --CASE Check
  New_Line;   PUT_LINE("CASE Check");
  i1 := -1;
  PUT("Selector value ="); Put(i1); New_Line;
  CASE i1 is
     WHEN    1 | 2    =>    PUT ("Case is 1 or 2 ...");
                            PUT_LINE("that's OK");
     WHEN    3        =>    PUT_LINE("Case = 3, ");
     WHEN    4        =>    PUT_LINE("Case = 4, ");
     WHEN  OTHERS     =>    PUT ("Case is ");
                            PUT_LINE("Others!");
  END CASE;

  New_Line;   PUT_LINE("END OF TEST");

END Test1;
