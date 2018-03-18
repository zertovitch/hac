with HAC_Pack;  use HAC_Pack;

procedure Test1 is

  --This is the initial test program for the Small-Ada Compiler
  --   Small-Ada is a restircted subset of the full Ada Language
  --   The Small-Ada compiler was derived from the Co-Pascal
  --       compiler which was in turn was derived from Pascal-S

  --Declarations

  Ten: constant Integer := 10;     -- Integer    (*  test  *)
  Twenty: constant  := 20;
  TenpOne: constant := 10.1;        -- Float
  CA: constant  Character := 'A';          -- Character
  CE: constant  Character := 'E';          -- Character
  B1, B2: constant Boolean:= True;        -- Boolean

  type Type1 is
     record
        X : Integer;
        Y : Float;
     end record;
  --TYPE       Type2 IS String(1..10);
  type Type3 is array (1 .. 4) of Type1;

  --TYPE       Type4 IS RECORD
  --                 ay: array(1..Ten) of integer;
  --                END RECORD;
  type Day is
        (Sun,
         Mon,
         Tue,
         Wed,
         Thu,
         Fri,
         Sat);

  I1, I2, I3:  Integer := 99;
  I4: constant Integer := -I1 + 10;
  I5, I6, I7:  Integer;
  R1: Float := 1.23;
  R2: Float := TenpOne;
  C2: constant Character := '+';
  C1 : constant Character := '+';
  C3: constant Character := CA;
  B3, B4, B5: Boolean := False;
  A:  array(1..5) of Integer;
  WeekDay: Day;

  function  Add(X, Y: Integer) return Integer is
    Value: Integer;
  begin
    Value :=  ( X + Y );
    return Value;
  end Add;

  procedure IComp(X, Y: Integer) is
  begin
    if X <= Y then Put("In "); Put_Line("Order");
    else Put_Line("Reverse Order");  end if;
  end IComp;

begin
  --CONSTANT Check
  Put_Line("CONSTANT Check");
  Put("Ten     = ");  Put_Line(Ten);
  Put("Twenty  = ");  Put_Line(Twenty);
  Put("TenpOne = ");  Put_Line(TenpOne,10,1);
  Put("CA      = ");  Put(CA); New_Line;
  Put("B1, B2  = ");  Put(B1); Put(' '); Put_Line(B2);
  New_Line;

  Put_Line("Initialization Check");
  Put("i1, i2, i3  "); Put(I1); Put(I2); Put(I3); New_Line;
  Put("i4       ");    Put(I4); New_Line;
  Put_Line("r1, r2 (1.23, 10.1): ");
  Put(R1); Put(R2); New_Line;
  Put_Line("c1, c2, c3 (++A):");
  Put(C1); Put(C2); Put(C3); New_Line;
  Put_Line("b1, .., b5 (T,T,F,F,F):");
  Put("   "); Put(B1); Put(' '); Put(B2); Put(' '); Put(B3); Put(' '); Put(
    B4); Put(' '); Put(B5); New_Line;
  B3:= True;
  B4:= True;
  B5:= B3 = not B4;
  Put_Line("b1, .., b5 (T,T,T,T,F):");
  Put("   "); Put(B1); Put(' '); Put(B2); Put(' '); Put(B3); Put(' '); Put(
    B4); Put(' '); Put(B5); New_Line;
  New_Line;

  Put_Line("ARITHMETIC Check");
  I1 := 13;             Put(I1); Put_Line(" (i1:= 13)");
  I2 := Ten;            Put(I2); Put_Line(" (i2:= 10)");
  I3 := I1 + I2 + 5;    Put(I3); Put_Line(" (i3:= i1+i2+5 = 28)");
  I3 := I1 - Twenty;    Put(I3); Put_Line(" (i3:= i1-20 = -7)");
  I3 := I2 * I1;        Put(I3); Put_Line(" (i3:= i1*i2 = 130)");
  I3 := I1 / 5;         Put(I3); Put_Line(" (i3:= i1 DIV 5)");
  I3 := I1 mod 5;       Put(I3); Put_Line(" (i3:= i1 MOD 5)");
  R1 := 20.4;           Put(R1); Put_Line(" (r1:= 20.4)");
  R2 := R1/2.0;         Put(R2); Put_Line(" (r2:= r1/2.0 = 10.2)");
  Put(10.0 * TenpOne); Put_Line(" (10*10.1)");
  New_Line;

  Put_Line("LOOP and ARRAY Check");
  I1 := 10;
  Put_Line("Single FOR LOOP 1..5: filling array");
  for I in 1 .. 5 loop
    A ( I ) := I * 2;
  end loop;
  Put_Line("Single FOR LOOP 1..5: reading array");
  for N in 1 .. 5 loop
    Put("   n = "); Put(N);
    Put("; a(n) = n*2 = ");
    Put(A(N)); New_Line;
  end loop;
  New_Line;
  Put_Line("Double FOR LOOP: 1..2, then A..E");
  for I in 1 .. 2 loop
    Put("   Line: "); Put(I); Put(' ');
    for J in CA .. CE  loop
      Put(J);
    end loop;
    New_Line;
  end loop;
  Put_Line("Reverse FOR LOOP 1..7");
  for I1 in reverse 1..7 loop
    Put(I1);
  end loop;
  Put("  i1 = "); Put(I1);
  New_Line;
  New_Line;

  Put_Line("WHILE Check 1..5");
  I1 := 0;
  while I1 < 5 loop
    I1 := I1 + 1;
    Put(I1);
  end loop;
  New_Line;

  --REPEAT Check
  New_Line;   Put_Line("REPEAT Check");
  loop
    Put("Rpt  ");
    I1 := I1 + 1;
    exit when I1 = Ten;
  end loop;
  New_Line;

  --Function and Procedure Check
  New_Line;   Put_Line("FUNCTION and PROCEDURE Check");
  Put("   compare 10 to 20 ");  IComp(Ten, Twenty);
  Put("   compare 20 to 10 ");  IComp(Twenty, Ten);
  Put(Ten);  Put(5);  Put("     ADD = "); Put(Add(Ten,5)); New_Line;
  New_Line;

  Put_Line("IF and BOOLEAN Check");
  if Ten > Twenty then
    Put(Ten); Put('>'); Put(Twenty); Put_Line("O_o: bug with > operator");
  else
    Put(Ten); Put("<="); Put(Twenty); Put_Line(" > operator works.");
  end if;

  I1 := 99;
  Put(I1); Put(" compared to 10 and 20 is :  ");
  if I1 < 10 then
    Put_Line("the smallest");
  elsif I1 < 20 then
    Put_Line("in the middle");
  else
    Put_Line("the largest");
  end if;

  if Twenty > 10 then
    Put(Twenty); Put('>'); Put(Ten); New_Line;
  end if;

  if not False then Put_Line ("(1/2) NOT is OK"); else Put_Line (
      "(1/2) NOT is not OK"); end if;
  if not True  then Put_Line ("(2/2) NOT is not OK"); else Put_Line (
      "(2/2) NOT is OK"); end if;

  if False and False then Put_Line ("(1/4) AND is not OK"); else Put_Line (
      "(1/4) AND is OK"); end if;
  if True  and False then Put_Line ("(2/4) AND is not OK"); else Put_Line (
      "(2/4) AND is OK"); end if;
  if False and True  then Put_Line ("(3/4) AND is not OK"); else Put_Line (
      "(3/4) AND is OK"); end if;
  if True  and True  then Put_Line ("(4/4) AND is OK"); else Put_Line (
      "(4/4) AND is not OK"); end if;

  if False or False then Put_Line ("(1/4)  OR is not OK"); else Put_Line (
      "(1/4)  OR is OK"); end if;
  if True  or False then Put_Line ("(2/4)  OR is OK"); else Put_Line (
      "(2/4)  OR is not OK"); end if;
  if False or True  then Put_Line ("(3/4)  OR is OK"); else Put_Line (
      "(3/4)  OR is not OK"); end if;
  if True  or True  then Put_Line ("(4/4)  OR is OK"); else Put_Line (
      "(4/4)  OR is not OK"); end if;

  if False xor False then Put_Line ("(1/4) XOR is not OK"); else Put_Line (
      "(1/4) XOR is OK"); end if;
  if True  xor False then Put_Line ("(2/4) XOR is OK"); else Put_Line (
      "(2/4) XOR is not OK"); end if;
  if False xor True  then Put_Line ("(3/4) XOR is OK"); else Put_Line (
      "(3/4) XOR is not OK"); end if;
  if True  xor True  then Put_Line ("(4/4) XOR is not OK"); else Put_Line (
      "(4/4) XOR is OK"); end if;

  New_Line;

  --CASE Check
  Put_Line("CASE Check");
  I1 := -1;
  Put("Selector value ="); Put(I1); New_Line;
  case I1 is
    when    1 | 2    =>    Put ("Case is 1 or 2 ...");
      Put_Line("that's OK");
    when    3        =>    Put_Line("Case = 3, ");
    when    4        =>    Put_Line("Case = 4, ");
    when  others     =>    Put ("Case is ");
      Put_Line("Others (not 1,2,3,4)");
  end case;

  New_Line;   Put_Line("END OF TEST");

end Test1;
