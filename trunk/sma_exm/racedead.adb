with SMALL_SP; use SMALL_SP;
procedure RACEDead is
------------------------------------------------------------------------------
-- Sort race demonstration program for THE AVL PARALLEL MONITORING           -
-- WITH                                SmallAda Version 2.0.                 -
------------------------------------------------------------------------------
   STIME : FLOAT;
   task BUBBLE_SORT is
     entry S(r : in INTEGER; t : in FLOAT);
     entry X;
   end BUBBLE_SORT;

   task INSERT_SORT is
     entry S(r : in INTEGER; t : in FLOAT);
     entry X;
   end INSERT_SORT;

   task SHELL_SORT is
     entry S(r : in INTEGER; t : in FLOAT);
     entry X;
   end SHELL_SORT;

   task SCREEN is
     entry P(X,Y : in INTEGER; C : in CHARACTER);
   end SCREEN;
   task body SCREEN is


   begin
     loop
       select
       accept P(X,Y : in INTEGER;
                  C : in CHARACTER) do
         CURSORAT(X,Y);
         PUT(C);
       end P;
       end select ;
     end loop;
   end SCREEN;

   task body BUBBLE_SORT is
    B : STRING(0..5);
    TEMP : CHARACTER;
    I,J : INTEGER;
    ROW : INTEGER;
    STIME : FLOAT;
   begin
       B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
       accept S(r : in INTEGER; t : in FLOAT) do
         ROW := r;
         STIME := t;
       end S;
       for k in 0..5 loop SCREEN.P(ROW,14+k,B(k)); end loop;
       delay STIME - CLOCK;
       I:= 5;
       while (I > 0) loop
         J:= 0;
         while (J < I) loop
           if B(J) > B(J+1) then
             TEMP:= B(J+1);
             B(J+1):= B(J);
             B(J):= TEMP;
             SCREEN.P(ROW,14+J  ,B(J  ));
             SCREEN.P(ROW,14+J+1,B(J+1));
           end if;
           J:= J+1;
         end loop;
         I:= I-1;
       end loop;
       accept X;
   end BUBBLE_SORT;


   task body INSERT_SORT is
     B : STRING(0..5);
     TEMP : CHARACTER;
     J : INTEGER;
     STOP : BOOLEAN;
     ROW : INTEGER;
     STIME : FLOAT;
   BEGIN
     B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
     accept S(r : in INTEGER; t : in FLOAT) do
       ROW := r;
       STIME := t;
     end S;
     for k in 0..5 loop SCREEN.P(ROW,14+k,B(k)); end loop;
     delay STIME - CLOCK;
     for I in 0..5 loop
       TEMP := B(I); J:= I; STOP := FALSE;
       while (J > 0) and (not STOP) loop
         J := J - 1;
         if (B(J) > TEMP) then
            B(J+1):= B(J);
         else
            B(J+1):= TEMP;
            STOP := TRUE;
         end if;
         SCREEN.P(ROW, 14+(J+1) ,B(J+1));
       end loop;
       if (not STOP) then
          B(0):= TEMP;
          SCREEN.P(ROW ,14 ,B(0));
       end if;
     end loop;
     accept X;
  end INSERT_SORT;


   task body SHELL_SORT is
     B : STRING(0..5);
     TEMP : CHARACTER;
     I,J,STEP : INTEGER;
     STEPSIZE : array(1..4) OF INTEGER;
     STOP : BOOLEAN;
     ROW : INTEGER;
     STIME : FLOAT;

   begin
     B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
     accept S(r : in INTEGER; t : in FLOAT) do
       ROW := r;
       STIME := t;
     end S;
     for k in 0..5 loop SCREEN.P(ROW,14+k,B(k)); end loop;
                    -- 'steps' contains decreasing increments for each
                    -- pass. The last pass has increment 1.
     STEPSIZE(4) := 1;
     for PASS in reverse 1..3 loop
         STEPSIZE(PASS):= 2*STEPSIZE(PASS+1);
     end loop;

     delay STIME - CLOCK;
     for PASS in 1..4 loop
       STEP := STEPSIZE(PASS);
                    -- Do a straight insertion sort with 'step' as
                    -- an increment instead of 1.
       I:= STEP;
       while (I <= 5) loop
         TEMP := B(I); J:= I; STOP:= FALSE;
         while (J > STEP-1) and (STOP /= TRUE) loop
           J := J - STEP;
           if B(J) > TEMP then
              B(J+STEP):= B(J);
              SCREEN.P(ROW, 14+J, TEMP);
           else
              B(J+STEP):= TEMP;
              STOP:= TRUE;
           end if;
           SCREEN.P(ROW, 14+(J+STEP) ,B(J+STEP));
         end loop;
         if (not STOP) then
            B(0):= TEMP;
            SCREEN.P(ROW, 14 ,B(0));
         end if;
         I := I + STEP;
       end loop;
     end loop; -- for pass in 1..npass
     accept X;
   end SHELL_SORT;

begin

  CURSORAT(3,1); PUT("Bubble:");
  CURSORAT(5,1); PUT("Insertion:");
  CURSORAT(7,1); PUT("Shell:");

  STIME := CLOCK + 10.0;

  BUBBLE_SORT.S(3,STIME);
  INSERT_SORT.S(5,STIME);
  SHELL_SORT.S(7,STIME);

  BUBBLE_SORT.X;
  INSERT_SORT.X;
  SHELL_SORT.X;
end RACEDead;
