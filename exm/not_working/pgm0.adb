with HAT; use HAT;

procedure PGM0 is

  TASK TELLER IS
     ENTRY MAKE_DEP(CUST_ID : INTEGER;
                    AMOUNT  : Real);
  END TELLER;

  TASK CUST_C;
  TASK CUST_B;
  TASK CUST_A;

  TYPE CUST_RECORD IS
    RECORD
      ID      : INTEGER;
      BALANCE : Real;
    END RECORD;

  ACCOUNT : ARRAY (1 .. 5) OF CUST_RECORD;

  procedure Show is
  begin
    for i in account'Range loop
      Put_Line (+"#" & i & "; ID: " & account (i).ID & "; balance: " & account (i).balance);
    end loop;
    New_Line;
  end;

  TASK BODY CUST_C IS
  BEGIN
    TELLER.MAKE_DEP (435,75.50);
  END CUST_C;

  TASK BODY CUST_B IS
  BEGIN
    TELLER.MAKE_DEP (878,100.0);
  END CUST_B;

  TASK BODY CUST_A IS
  BEGIN
    TELLER.MAKE_DEP (354,50.00);
  END CUST_A;

  TASK BODY TELLER IS
  BEGIN
    ACCOUNT (1).ID      := 125;
    ACCOUNT (1).BALANCE := 400.50;
    ACCOUNT (2).ID      := 354;
    ACCOUNT (2).BALANCE := 75.33;
    ACCOUNT (3).ID      := 435;
    ACCOUNT (3).BALANCE := 137.95;
    ACCOUNT (4).ID      := 878;
    ACCOUNT (4).BALANCE := 557.00;
    ACCOUNT (5).ID      := 589;
    ACCOUNT (5).BALANCE := 235.75;
    Show;

    LOOP
      SELECT
        ACCEPT MAKE_DEP (CUST_ID : INTEGER;
                         AMOUNT  : Real) DO
           for I in account'Range loop
             IF ACCOUNT(I).ID = CUST_ID THEN
                ACCOUNT(I).BALANCE :=
                   ACCOUNT(I).BALANCE
                   + AMOUNT;
               Show;
               exit;
             END IF;
           END LOOP;
        END MAKE_DEP;
      OR
        TERMINATE;
      END SELECT;
    END LOOP;
  END TELLER;

BEGIN
   NULL;
END;
