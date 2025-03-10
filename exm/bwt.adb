--  Burrows-Wheeler Transform: block-sorting
--  preprocessing for improving data compression.
--  This technique is used in the BZip2 format.
--
--  https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform
--
--  HAC example derived from extras/bwt* files in Zip-Ada project.
--
with HAT;

procedure BWT is
  use HAT;

  n : constant := 52;
  default : constant VString := +"Mary had a little lamb, its fleece was white as snow";

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
  Passes :
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
          if b (j) > temp then
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
    end loop Passes;
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
  row_index : Positive;
  line : VString;

begin
  --  Put_Line (+"Enter " & n & " characters (until and including '*' position");
  --  Put_Line ((n - 1) * '.' & '*');
  --  Get_Line (line);
  line := default;
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
  Show (m, +"unsorted ( (i-1) rotations for row i )", 1);
  -----------------
  --  Transform  --
  -----------------
  Shell_Sort (m);
  for i in 1 .. n loop
    t (i) := m (i)(n);
    if m (i) = s then
      row_index := i;  --  Found row with the message with 0 rotation.
    end if;
  end loop;
  Show (m, +"sorted", row_index);
  --
  Put_Line ("BWT output (last column of matrix):");
  Put_Line (n * '-');
  Put_Line ("t = " & t);
  Put_Line (n * '-');
  Put_Line (+"Index of row containing the original message is: " & row_index);
  --------------------
  --  De-transform  --
  --------------------
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
    --  of the character on sorted partial row i (1 character).
    --  This gives the full list of pairs.
    --
    --  After 2nd sorting (end of iteration #2), t(i) is also the correct
    --  predecessor each sorted pair.
    --  We have then the list of all triplets. And so on.
    --
    for i in 1 .. n loop
      m (i)(1) := t (i);
    end loop;
    Show (m, +"insert #" & iter, 0);
    Shell_Sort (m);
    Show (m, +"sort #" & iter, 0);
  end loop Shift_Insert_Sort;
  --  After iteration n we have a sorted list of all rotated
  --  versions of the original string. The table is identical
  --  to the table after encoding.
  --  The original string is at row 'row_index'.
  u := m (row_index);
  --
  --  Output of table.
  --
  Show (m, +"reconstructed", row_index);
  --
  Put_Line ("BWT output de-transformed.");
  --
  Put_Line (n * '-');
  Put_Line (u);
end BWT;
