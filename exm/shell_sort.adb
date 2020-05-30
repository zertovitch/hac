with HAC_Pack; use HAC_Pack;  -- CS159-10 - FAll/1990 - ARthur Vargas Lopes

procedure Shell_Sort is

  -- ch : character;

  procedure Shell is
    b : String (1 .. 26);
    i, j, step : Integer;
    step_size : array(1..4) of Integer;
    stop : Boolean;
    temp : Character;
  begin
    b := "ZYXWVUTSRQPONMLKJIHGFEDCBA";
    Put_Line ("Shell Sort");
    New_Line;
    Put_Line ("String at start:");
    Put_Line ("----------------");
    Put_Line (b);
    New_Line;
    -- Put_Line(B);
    -- 'steps' contains decreasing increments for each
    -- pass. The last pass has increment 1.
    step_size(4) := 1;
    for pass in reverse 1..3 loop
      step_size(pass):= 2*step_size(pass+1);
    end loop;
    for pass in 1..4 loop
      -- cursorat(14,45);
      Put("Pass: "); Put(pass);
      New_Line;
      step := step_size(pass);
      Put("Step: "); Put(step);
      New_Line;
        -- Do a straight insertion sort with 'step' as
        -- an increment instead of 1.
      i:= step + 1;
      while i <= 26 loop
        --cursorat(15,45);
        New_Line;
        Put("I:    "); Put(i); New_Line;
        temp := b(i);
        j:= i;
        stop:= False;
        while (j > step) and not stop loop
          j := j - step;
          -- cursorat(16,45); put("J:    "); put(J);
          if b(j) > temp then
            b(j+step):= b(j);
            -- cursorat(15,14+j);
            Put(temp);
          else
            b(j+step):= temp;
            stop:= True;
          end if;
          --  CursorAt(15,14+(j+step)); Put(b(j+step));
        end loop;
        if not stop then
          b(1):= temp;
          --  CursorAt(15,14+0);
          Put(temp);
        end if;
        i := i + step;
      end loop;
      New_Line;
    end loop; -- for pass in 1..npass
    New_Line;
    Put_Line ("Result of Shell Sort:");
    Put_Line ("---------------------");
    Put_Line (b);
  end Shell;

begin
  Shell;
  -- New_Line;
  -- PUT("Press enter to proceed...");
  -- GET(ch);
end Shell_Sort;
