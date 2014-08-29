WITH SMALL_SP;  USE SMALL_SP;

PROCEDURE   Test1 IS

  --This is the initial test program for the Small-Ada Compiler
  --   Small-Ada is a restircted subset of the full Ada Language
  --   The Small-Ada compiler was derived from the Co-Pascal
  --       compiler which was in turn was derived from Pascal-S

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
    b3, b4, b5: Boolean := false;
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
  PUT_LINE("r1, r2 (1.23, 10.1): ");
  Put(r1); Put(r2); New_Line;
  PUT_LINE("c1, c2, c3 (++A):");
  Put(c1); Put(c2); Put(c3); New_Line;
  PUT_LINE("b1, .., b5 (T,T,F,F,F):");
  Put("   "); Put(b1); Put(' '); Put(b2); Put(' '); Put(b3); Put(' '); Put(b4); Put(' '); Put(b5); New_Line;
  b3:= True;
  b4:= True;
  b5:= b3 = not b4;
  PUT_LINE("b1, .., b5 (T,T,T,T,F):");
  Put("   "); Put(b1); Put(' '); Put(b2); Put(' '); Put(b3); Put(' '); Put(b4); Put(' '); Put(b5); New_Line;
  New_LINE;

  PUT_LINE("ARITHMETIC Check");
  i1 := 13;             Put(i1); PUT_LINE(" (i1:= 13)");
  i2 := Ten;            Put(i2); PUT_LINE(" (i2:= 10)");
  i3 := i1 + i2 + 5;    Put(i3); PUT_LINE(" (i3:= i1+i2+5 = 28)");
  i3 := i1 - Twenty;    Put(i3); PUT_LINE(" (i3:= i1-20 = -7)");
  i3 := i2 * i1;        Put(i3); PUT_LINE(" (i3:= i1*i2 = 130)");
  i3 := i1 / 5;         Put(i3); PUT_LINE(" (i3:= i1 DIV 5)");
  i3 := i1 MOD 5;       Put(i3); PUT_LINE(" (i3:= i1 MOD 5)");
  r1 := 20.4;           Put(r1); PUT_LINE(" (r1:= 20.4)");
  r2 := r1/2.0;         Put(r2); PUT_LINE(" (r2:= r1/2.0 = 10.2)");
  PUT(10.0 * tenpone); Put_Line(" (10*10.1)");
  New_LINE;

  PUT_LINE("LOOP and ARRAY Check");
  i1 := 10;
  PUT_LINE("Single FOR LOOP 1..5: filling array");
  for i IN 1 .. 5 LOOP
     a ( i ) := i * 2;
  END LOOP;
  PUT_LINE("Single FOR LOOP 1..5: reading array");
  for n IN 1 .. 5 LOOP
     Put("   n = "); Put(n);
     Put("; a(n) = n*2 = ");
     Put(a(n)); New_Line;
  END LOOP;
  New_LINE;
  PUT_LINE("Double FOR LOOP: 1..2, then A..E");
  FOR i IN 1 .. 2 LOOP
     PUT("   Line: "); Put(i); Put(' ');
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
  PUT("   compare 10 to 20 ");  IComp(ten, twenty);
  PUT("   compare 20 to 10 ");  IComp(twenty, ten);
  PUT(ten);  PUT(5);  PUT("     ADD = "); Put(add(ten,5)); New_Line;
  New_Line;

  PUT_LINE("IF and BOOLEAN Check");
  IF ten > twenty THEN
     PUT(ten); Put('>'); Put(twenty); Put_Line("O_o: bug with > operator");
  ELSE
     PUT(ten); Put("<="); Put(twenty); Put_Line(" > operator works.");
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

  if not False then Put_Line ("(1/2) NOT is OK"); else Put_Line ("(1/2) NOT is not OK"); end if;
  if not True  then Put_Line ("(2/2) NOT is not OK"); else Put_Line ("(2/2) NOT is OK"); end if;

  if False and False then Put_Line ("(1/4) AND is not OK"); else Put_Line ("(1/4) AND is OK"); end if;
  if True  and False then Put_Line ("(2/4) AND is not OK"); else Put_Line ("(2/4) AND is OK"); end if;
  if False and True  then Put_Line ("(3/4) AND is not OK"); else Put_Line ("(3/4) AND is OK"); end if;
  if True  and True  then Put_Line ("(4/4) AND is OK"); else Put_Line ("(4/4) AND is not OK"); end if;

  if False or False then Put_Line ("(1/4)  OR is not OK"); else Put_Line ("(1/4)  OR is OK"); end if;
  if True  or False then Put_Line ("(2/4)  OR is OK"); else Put_Line ("(2/4)  OR is not OK"); end if;
  if False or True  then Put_Line ("(3/4)  OR is OK"); else Put_Line ("(3/4)  OR is not OK"); end if;
  if True  or True  then Put_Line ("(4/4)  OR is OK"); else Put_Line ("(4/4)  OR is not OK"); end if;

  if False xor False then Put_Line ("(1/4) XOR is not OK"); else Put_Line ("(1/4) XOR is OK"); end if;
  if True  xor False then Put_Line ("(2/4) XOR is OK"); else Put_Line ("(2/4) XOR is not OK"); end if;
  if False xor True  then Put_Line ("(3/4) XOR is OK"); else Put_Line ("(3/4) XOR is not OK"); end if;
  if True  xor True  then Put_Line ("(4/4) XOR is not OK"); else Put_Line ("(4/4) XOR is OK"); end if;
  
  New_Line;

  --CASE Check
  PUT_LINE("CASE Check");
  i1 := -1;
  PUT("Selector value ="); Put(i1); New_Line;
  CASE i1 is
     WHEN    1 | 2    =>    PUT ("Case is 1 or 2 ...");
                            PUT_LINE("that's OK");
     WHEN    3        =>    PUT_LINE("Case = 3, ");
     WHEN    4        =>    PUT_LINE("Case = 4, ");
     WHEN  OTHERS     =>    PUT ("Case is ");
                            PUT_LINE("Others (not 1,2,3,4)");
  END CASE;

  New_Line;   PUT_LINE("END OF TEST");

END Test1;
