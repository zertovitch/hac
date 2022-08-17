--  Burrows-Wheeler transform: block-sorting
--  preprocessing for improving data compression.
--  This technique is used in the BZip2 format.
--
--  https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--
--  HAC example derived from extras/bwt* files in Zip-Ada project.
--
--  See "!!" for improvements to be done in HAC.
--
with HAT; use HAT;

procedure BWT is

  n : constant := 9;

  subtype Row is String (1 .. n);

  type Table is array (1 .. n) of Row;

  procedure Shell_Sort (b : in out Table) is
    i, j, step : Integer;
    step_size : array (1 .. 4) of Integer;
    stop : Boolean;
    temp : Row;
  begin
    --  'steps' contains decreasing increments for each
    --  pass. The last pass has increment 1.
    step_size (4) := 1;
    for pass in reverse 1 .. 3 loop
      step_size (pass) := 2 * step_size (pass + 1);
    end loop;
    for pass in 1 .. 4 loop
      step := step_size (pass);
      --  Do a straight insertion sort with 'step' as
      --  an increment instead of 1.
      i := step + 1;
      while i <= n loop
        temp := b (i);
        j := i;
        stop := False;
        while j > step and not stop loop
          j := j - step;
          if +b (j) > +temp then
            --  !! HAC: so far we need unary "+" for converting to VString.
            --  TBD in HAC: ">" for array-of-discrete comparisons (RM 4.5.2 (26)).
            b (j + step) := b (j);
          else
            b (j + step) := temp;
            stop := True;
          end if;
        end loop;
        if not stop then
          b (1) := temp;
        end if;
        i := i + step;
      end loop;
    end loop; -- for pass in 1..npass
  end Shell_Sort;

  procedure Show (m : Table; message : VString; original : Natural) is
  begin
    Put_Line ("---- BWT Table: " & message & " ----");
    for i in 1 .. n loop
      if i = original then
        Put ("--original-->");
      else
        Put ("             ");
      end if;
      Put (i, 4);
      Put (' ');
      for j in 1 .. n loop
        Put (m (i)(j));
      end loop;
      New_Line;
    end loop;
  end Show;

  s, t, u : Row;
  m : Table;
  index : Positive;
  line : VString;

begin
  --  Put_Line (+"Enter " & n & " characters (until and including '*' position");
  --  Put_Line ((n - 1) * '.' & '*');
  --  Get_Line (line);
  line := +"Barbapapa";
  --
  for i in 1 .. n loop
    s (i) := Element (line, i);
  end loop;
  New_Line;
  --  Fill the matrix
  for i in 1 .. n loop
    for j in 1 .. n loop
      m (i)(j) := Element (line, (1 + (j - 1 + i - 1) mod n));
    end loop;
  end loop;
  --
  Show (m, +"unsorted (rotations)", 1);
  Shell_Sort (m);
  for i in 1 .. n loop
    t (i) := m (i)(n);
    if +m (i) = +s then
      --  !! HAC : without VString (remove the "+"): why does the "=" comparison find incompatible types?
      index := i;  --  Found row with the message with 0 rotation.
    end if;
  end loop;
  Show (m, +"sorted", index);
  --
  Put_Line ("BWT output with transform:");
  Put_Line (n * '-');
  Put_Line (t);
  Put_Line (n * '-');
  Put_Line (+"Index of row containing the original message is: " & index);
  --  De-transform
  for i in 1 .. n loop
    for j in 1 .. n loop
      m (i)(j) := ' ';
    end loop;
  end loop;
  Shift_Insert_Sort :
  for iter in 1 .. n loop
    --  Shift columns right
    for i in 1 .. n loop
      for j in reverse 2 .. n loop
        m (i)(j) := m (i)(j - 1);
      end loop;
    end loop;
    --  Insert transformed string t as first column (again and again).
    --
    --  The miracle: after iteration #1, t(i) is the correct predecessor
    --  of the character on sorted row i. This gives the full list of pairs.
    --  After 2nd sorting (end of iteration #2), t(i) is also the correct
    --  predecessor each sorted pair.
    --  We have then the list of all triplets. And so on.
    --
    for i in 1 .. n loop
      m (i)(1) := t (i);
    end loop;
    Show (m, +"insert #" & iter, 0);
    Shell_Sort (m);
  end loop Shift_Insert_Sort;
  --  After iteration n we have a sorted list of all rotated
  --  versions of the original string. The table is identical
  --  to the table after encoding.
  --  The original string is at row 'index'.
  u := m (index);
  --
  --  Output of table.
  --
  Show (m, +"reconstructed", index);
  --
  Put_Line ("BWT output de-transformed.");
  --
  Put_Line (n * '-');
  Put_Line (u);
end BWT;
