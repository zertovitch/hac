with SMALL_SP; use SMALL_SP;
procedure PGM0 is
   TASK TELLER IS
      ENTRY MAKE_DEP(CUST_ID : INTEGER;
                     AMOUNT  : FLOAT);
   END TELLER;

   TASK CUST_C;

   TASK CUST_B;

   TASK CUST_A;

TYPE CUST_RECORD IS
   RECORD
      ID      : INTEGER;
      BALANCE : FLOAT;
   END RECORD;

ACCOUNT : ARRAY(1..5) OF CUST_RECORD;

TASK BODY CUST_C IS
BEGIN
   TELLER.MAKE_DEP(435,75.50);
END CUST_C;

TASK BODY CUST_B IS
BEGIN
   TELLER.MAKE_DEP(887,100.0);
END CUST_B;

TASK BODY CUST_A IS
BEGIN
   TELLER.MAKE_DEP(354,50.00);
END CUST_A;

TASK BODY TELLER IS
   I : INTEGER;
BEGIN
   ACCOUNT(1).ID      := 125;
   ACCOUNT(1).BALANCE := 400.50;
   ACCOUNT(2).ID      := 354;
   ACCOUNT(2).BALANCE := 75.33;
   ACCOUNT(3).ID      := 435;
   ACCOUNT(3).BALANCE := 137.95;
   ACCOUNT(4).ID      := 878;
   ACCOUNT(4).BALANCE := 557.00;
   ACCOUNT(5).ID      := 589;
   ACCOUNT(5).BALANCE := 235.75;

   LOOP
      SELECT
         ACCEPT MAKE_DEP(CUST_ID : INTEGER;
                         AMOUNT  : FLOAT) DO
            I := 1;
            WHILE ACCOUNT(I).ID /= CUST_ID LOOP
               I := I + 1;
            END LOOP;
            IF ACCOUNT(I).ID = CUST_ID THEN
               ACCOUNT(I).BALANCE :=
                  ACCOUNT(I).BALANCE
                  + AMOUNT;
            END IF;
         END MAKE_DEP;
      OR
         TERMINATE;
      END SELECT;
   END LOOP;
END TELLER;
BEGIN
   NULL;
END;
