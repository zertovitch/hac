with HAC_Pack; use HAC_Pack;  -- CS159-10 - FAll/1990 - ARthur Vargas Lopes

procedure Shell_Sort is

  -- ch : character;
  procedure Shell is
    B : STRING(1..26);
    I,J,STEP : INTEGER;
    STEPSIZE : array(1..4) OF INTEGER;
    STOP : BOOLEAN;
    TEMP : character;
   begin
     B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
     for k in 1..26 loop
        PUT(B(k));
     end loop;
     new_line;
     -- Put_Line(B);
     -- 'steps' contains decreasing increments for each
     -- pass. The last pass has increment 1.
     STEPSIZE(4) := 1;
     for PASS in reverse 1..3 loop
         STEPSIZE(PASS):= 2*STEPSIZE(PASS+1);
     end loop;
     for PASS in 1..4 loop
       -- cursorat(14,45);
       put("Pass: "); put(pass);
       new_line;
       STEP := STEPSIZE(PASS);
       put("Step: "); put(step);
       new_line;
         -- Do a straight insertion sort with 'step' as
         -- an increment instead of 1.
       I:= STEP + 1;
       while I <= 26 loop
         --cursorat(15,45);
         new_line;
         put("I:    "); put(i); new_line;
         TEMP := B(I);
         J:= I;
         STOP:= FALSE;
         while (J > STEP) and not STOP loop
           J := J - STEP;
           -- cursorat(16,45); put("J:    "); put(J);
           if B(J) > TEMP then
              B(J+STEP):= B(J);
              -- cursorat(15,14+j);
              put(temp);
           else
              B(J+STEP):= TEMP;
              STOP:= TRUE;
           end if;
           cursorat(15,14+(J+STEP)); put(B(J+STEP));
         end loop;
         if not STOP then
            B(1):= TEMP;
            cursorat(15,14+0);
            put(temp);
         end if;
         I := I + STEP;
       end loop;
       new_line;
     end loop; -- for pass in 1..npass
     New_Line;
     Put_Line("Result:");
     for k in 1..26 loop
        Put(B(k));
     end loop;
     new_line;
   end Shell;

begin
  -- CURSORAT(15,1);
  PUT_Line("Shell sort:");
  Shell;
  -- CURSORAT(17,1);
  New_Line;
  -- PUT("Press enter to proceed...");
  -- GET(ch);
end Shell_Sort;
