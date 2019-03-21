--  Burrows–Wheeler transform: block-sorting
--  preprocessing for improving data compression.
--
--  https://en.wikipedia.org/wiki/Burrows%E2%80%93Wheeler_transform

--  !! Fails on HAC (2018-11-07) because the ">" doesn't seem to
--     work on arrays. Works nicely on "real" compilers.
--     See extras/bwt* files in Zip-Ada project for newer developments.
--
with Hac_Pack; use Hac_Pack;

procedure BWT is

  n: constant := 10;

  type Row is array(1..n) of Character;  --  new String (1..n);
  --  type Row is String (1..n);  --  Bogus Ada (HAC...)

  type Table is array(1..n) of Row;

  procedure Shell_Sort(b : in out Table) is
    i, j, step : Integer;
    step_size : array(1..4) of Integer;
    stop : Boolean;
    temp : Row;
  begin
    -- 'steps' contains decreasing increments for each
    -- pass. The last pass has increment 1.
    step_size(4) := 1;
    for pass in reverse 1..3 loop
      step_size(pass):= 2*step_size(pass+1);
    end loop;
    for pass in 1..4 loop
      step := step_size(pass);
      -- Do a straight insertion sort with 'step' as
      -- an increment instead of 1.
      i:= step + 1;
      while i <= n loop
        temp := b(i);
        j:= i;
        stop:= False;
        while (j > step) and not stop loop
          j := j - step;
          if b(j) > temp then  --  !!  Comparison of strings seems to be wrong under HAC.
            b(j+step):= b(j);
          else
            b(j+step):= temp;
            stop:= True;
          end if;
        end loop;
        if not stop then
          b(1):= temp;
        end if;
        i := i + step;
      end loop;
    end loop; -- for pass in 1..npass
  end Shell_Sort;

  procedure Show (m : Table) is
  begin
    for i in 1 .. n loop  
      Put (i);
      Put (' ');
      for j in 1 .. n loop  
        Put (m(i)(j));  
      end loop;  
      new_line;  
    end loop;  
  end;

  s, t, u : Row;
  m : Table;
  index: Integer; --  !!Positive; (no range in HAC so far)

begin
  for i in 1 .. n loop
    Get (s(i));
  end loop;
  New_Line;
  for i in 1 .. n loop
    for j in 1 .. n loop
      m(i)(j) := s(1 + (j-1 + i-1) mod n);
    end loop;
  end loop;
  Shell_Sort (m);
  Show (m);
  for i in 1 .. n loop
    t(i) := m(i)(n);
    if m(i) = s then
      index := i;  --  Found row with the message with 0 rotation.
    end if;
  end loop;
  --
  Put_Line ("Output with transform:");  
  for i in 1 .. n loop
    Put ('-');
  end loop;
  New_Line;
  for i in 1 .. n loop
    Put (t(i));
  end loop;
  New_Line;
  --  De-transform
  for i in 1 .. n loop
    for j in 1 .. n loop
      m(i)(n) := ' ';
    end loop;
  end loop;
  for iter in 1 .. n loop
    --  Shift columns right
    for i in 1 .. n loop
      for j in reverse 2 .. n loop
        m(i)(j) := m(i)(j-1);
      end loop;
    end loop;
    --  Insert transformed string t as first column.
    --
    --  The miracle: t(i) is the correct predecessor of
    --  each sorted character on row i (status at the end of
    --  iteration 1). This gives the full list of pairs.
    --  After sorting, t(i) is also the correct predecessor
    --  each sorted pair (end of iteration 2). We have then the
    --  list of all triplets. And so on.
    --
    for i in 1 .. n loop
      m(i)(1) := t(i);
    end loop;
    Shell_Sort (m);
  end loop;
  --  After iteration n we have a sorted list of all rotated
  --  versions of the original string. The table is identical
  --  to the table after encoding.
  --  The original string is at row 'index'.
  u := m (index);
  --
  --  Output of table.
  --
  Show (m);    
  --
  Put_Line ("Output de-transformed.");
  --
  New_Line;
  for i in 1 .. n loop
    Put ('-');
  end loop;
  New_Line;
  for i in 1 .. n loop
    Put (u (i));
  end loop;
end;
