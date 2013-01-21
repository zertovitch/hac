WITH SMALL_SP;  USE SMALL_SP;

PROCEDURE   Test_Pgm   IS

  --This is the initial test program for the Small-Ada Compiler
  --   Small-Ada is a restircted subset of the full Ada Language
  --   The Small-Ada compiler was derived from the Co-Pascal
  --       compiler which was in turn was derived from Pascas-S

  --Declarations

       Ten: CONSTANT INTEGER := 10;     -- Integer    (*  test  *)
       Twenty: CONSTANT  := 20;
       TenpOne: CONSTANT := 10.1;        -- Float
       CCh: CONSTANT   := 'A';          -- Character
       B1, B2: CONSTANT := True;        -- Boolean

TYPE   Type1 IS RECORD
                 x: integer;
                 y: Float;
                END RECORD;
TYPE       Type2 IS String(1..10);
TYPE       Type3 IS Array(1..4) of Type1;
TYPE       Type4 IS RECORD
                 ay: array(1..Ten) of integer;
                END RECORD;
TYPE       Day IS (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

    i1, i2, i3:  integer := 99;
    i4: integer := -i1 + 10;
    i5, i6, i7:  integer;
    r1: Float := 1.23;
    r2: Float := TenpOne;
    c1, c2:  character := '+';
    c3: character := CCh;
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
  PUT("TenpOne = ");  PUT_LINE(TenpOne:10:1);
  PUT("CCh     = ");  PUT_LINE(CCh:5);
  PUT("B1 and B2      = ");  PUT(B1);  PUT_LINE(B2);

  --Initialization Check
  PUT_LINE;  PUT_LINE("Initialization Check");
  PUT_LINE("i1 - i3  ", i1, i2, i3);
  PUT_LINE("i4       ", i4);
  PUT_LINE("r1 - r2  ", r1, r2);
  PUT_LINE("c1 - c3  ", c1, c2, c3);
  PUT_LINE("b1 - b5  ", b1, b2, b3, b4, b5);


  --ARITMNETIC Check
  PUT_LINE;  PUT_LINE("ARITHMETIC Check");
  i1 := 13;             PUT_LINE(i1, " (i1)");
  i2 := Ten;            PUT_LINE(i2, " (i2)");
  i3 := i1 + i2 + 5;    PUT_LINE(i3, " (i3=i1+i2+5)");
  i3 := i1 - Twenty;    PUT_LINE(i1, " (i3=i1-20)");
  i3 := i2 * i1;        PUT_LINE(i2, " (i3=i1*i2)");
  i3 := i1 / 5;         PUT_LINE(i3, " (i3=i1 DIV 5)");
  i3 := i1 MOD 5;       PUT_LINE(i3, " (i3=i1 MOD 5)");
  r1 := 20.4;           PUT_LINE(r1:6:2, " (r1)");
  r2 := r1/2;           PUT_LINE(r2:8:3, " (r1/2)");
  PUT_LINE(10 * tenpone : 10:5, " (10*10.1)");

  --LOOP and ARRAY Check
  PUT_LINE;   PUT_LINE("LOOP Check");
  i1 := 10;
  PUT_LINE("Single FOR LOOP 1..5");
  for i IN 1 .. 5 LOOP
     a ( i ) := i * 2;
  END LOOP;
  for n IN 1 .. 5 LOOP
     PUT(n:5, a(n):5);
  END LOOP;
  PUT_LINE;
  PUT_LINE("Double FOR LOOP 1..2 A..E");
  FOR i IN 1 .. 2 LOOP
     PUT("Line: ", i:2);
     for j IN 'A'..'E'  LOOP
        PUT(j:5);
     END LOOP;
     PUT_LINE;
  END LOOP;
  PUT_LINE("Reverse FOR LOOP 1..7");
  FOR i1 IN REVERSE 1..7 LOOP
     PUT(i1:5);
  END LOOP;
  PUT_LINE("  i1 = ", i1:2);
  PUT_LINE;

  --WHILE Check
  PUT_LINE;   PUT_LINE("WHILE Check");
  i1 := 0;
  While (i1 < 5) LOOP
      i1 := i1 + 1;
      PUT(i1:2);
  END LOOP;
  PUT_LINE;

  --REPEAT Check
  PUT_LINE;   PUT_LINE("REPEAT Check");
  LOOP
    PUT("Rpt  ");
    i1 := i1 + 1;
    Exit WHEN (i1 = Ten);
  END LOOP;
  PUT_LINE;

  --Function and Procedure Check
  PUT_LINE;   PUT_LINE("FUNCTION and PROCEDURE Check");
  PUT("compare 10 to 20 ");  IComp(ten, twenty);
  PUT("compare 20 to 10 ");  IComp(twenty, ten);
  PUT(ten:5);  PUT(5:5);  PUT_LINE("     ADD = ", add(ten,5):5);


  --IF Check
  PUT_LINE;   PUT_LINE("IF and BOOLEAN Check");
  IF ten > twenty THEN
     PUT_LINE(ten, '>', twenty);
  ELSE
     PUT_LINE(ten, '<', twenty);
  END IF;

  i1 := 99;
  PUT(i1, " compared to 10 and 20 is :  ");
  IF i1 < 10 THEN
     PUT_LINE("the smallest");
  ELSIF i1 < 20 THEN
     PUT_LINE("in the middle");
  ELSE
     PUT_LINE("the largest");
  END IF;

  IF twenty > 10 THEN
     PUT_LINE(twenty, '>', ten);
  END IF;

  IF (twenty > ten) AND True THEN
     PUT_LINE("AND OK");
  ELSE
     PUT_LINE("AND not OK");
  END IF;

  IF (ten > twenty) OR (Twenty > ten) THEN
     PUT_LINE ("OR OK");
  ELSE
     PUT_LINE ("OR not OK");
  END IF;

  IF NOT (ten > twenty) THEN
     PUT_LINE ("NOT OK");
  ELSE
     PUT_LINE ("NOT not OK");
  END IF;

  --CASE Check
  PUT_LINE;   PUT_LINE("CASE Check");
  i1 := -1;
  PUT_LINE("Selector value =", i1);
  CASE i1 of
     WHEN    1 | 2    =>    PUT ("Case is 1 or 2 ... ");
                            PUT_LINE("that's OK");
     WHEN    3        =>    PUT_LINE("Case = 3, ");
     WHEN    4        =>    PUT_LINE("Case = 4, ");
     WHEN  OTHERS     =>    PUT ("Case is ");
                            PUT_LINE("Others!");
  END CASE;

  PUT_LINE;   PUT_LINE("END OF TEST");

END Test_Pgm;
