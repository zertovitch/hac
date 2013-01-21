with SMALL_SP; use SMALL_SP;  -- CS159-10 - FAll/1990 - ARthur Vargas Lopes

procedure SHELL is
  ch : character;
  procedure SHELL_SORT is
    B : STRING(0..25);
    I,J,STEP : INTEGER;
    STEPSIZE : array(1..4) OF INTEGER;
    STOP : BOOLEAN;
    TEMP : character;
   begin
     B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
     for k in 0..25 loop
        CURSORAT(15,14+k);
        PUT(B(k));
     end loop;
     -- 'steps' contains decreasing increments for each
     -- pass. The last pass has increment 1.
     STEPSIZE(4) := 1;
     for PASS in reverse 1..3 loop
         STEPSIZE(PASS):= 2*STEPSIZE(PASS+1);
     end loop;
     for PASS in 1..4 loop
       cursorat(14,45); put("Pass: "); put(pass);
       STEP := STEPSIZE(PASS);
       put(" Step: "); put(step);
         -- Do a straight insertion sort with 'step' as
         -- an increment instead of 1.
       I:= STEP;
       while I <= 25 loop
         cursorat(15,45); put("I:    "); put(i);
         TEMP := B(I); J:= I; STOP:= FALSE;
         while (J > STEP-1) and (STOP /= TRUE) loop
           J := J - STEP;
           cursorat(16,45); put("J:    "); put(J);
           if B(J) > TEMP then
              B(J+STEP):= B(J); cursorat(15,14+j); put(temp);
           else
              B(J+STEP):= TEMP;
              STOP:= TRUE;
           end if;
           cursorat(15,14+(J+STEP)); put(B(J+STEP));
         end loop;
         if (not STOP) then
            B(0):= TEMP;
            cursorat(15,14+0); put(temp);
         end if;
         I := I + STEP;
       end loop;
     end loop; -- for pass in 1..npass
   end SHELL_SORT;

begin
  CURSORAT(15,1); PUT("Shell:");
  SHELL_SORT;
  CURSORAT(17,1); PUT("Press enter to proceed..."); GET(ch);
end SHELL;

