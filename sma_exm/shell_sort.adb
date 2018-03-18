with HAC_Pack; use HAC_Pack;  -- CS159-10 - FAll/1990 - ARthur Vargas Lopes

procedure Shell_Sort is

  -- ch : character;
  procedure Shell is
    B : String(1..26);
    I,J,STEP : Integer;
    STEPSIZE : array(1..4) of Integer;
    STOP : Boolean;
    TEMP : Character;
   begin
     B := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
     Put_Line ("Shell_Sort.");
     Put_Line ("Unsorted string:");
     Put_Line ("----------------");
     for k in 1..26 loop
        Put(B(k));
     end loop;
     New_Line;
     New_Line;
     -- Put_Line(B);
     -- 'steps' contains decreasing increments for each
     -- pass. The last pass has increment 1.
     STEPSIZE(4) := 1;
     for PASS in reverse 1..3 loop
         STEPSIZE(PASS):= 2*STEPSIZE(PASS+1);
     end loop;
     for PASS in 1..4 loop
       -- cursorat(14,45);
       Put("Pass: "); Put(PASS);
       New_Line;
       STEP := STEPSIZE(PASS);
       Put("Step: "); Put(STEP);
       New_Line;
         -- Do a straight insertion sort with 'step' as
         -- an increment instead of 1.
       I:= STEP + 1;
       while I <= 26 loop
         --cursorat(15,45);
         New_Line;
         Put("I:    "); Put(I); New_Line;
         TEMP := B(I);
         J:= I;
         STOP:= False;
         while (J > STEP) and not STOP loop
           J := J - STEP;
           -- cursorat(16,45); put("J:    "); put(J);
           if B(J) > TEMP then
              B(J+STEP):= B(J);
              -- cursorat(15,14+j);
              Put(TEMP);
           else
              B(J+STEP):= TEMP;
              STOP:= True;
           end if;
           CursorAt(15,14+(J+STEP)); Put(B(J+STEP));
         end loop;
         if not STOP then
            B(1):= TEMP;
            CursorAt(15,14+0);
            Put(TEMP);
         end if;
         I := I + STEP;
       end loop;
       New_Line;
     end loop; -- for pass in 1..npass
     New_Line;
     Put_Line("Result:");
     Put_Line("-------");
     for k in 1..26 loop
        Put(B(k));
     end loop;
     New_Line;
   end Shell;

begin
  Shell;
  -- New_Line;
  -- PUT("Press enter to proceed...");
  -- GET(ch);
end Shell_Sort;
