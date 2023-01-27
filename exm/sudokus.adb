package body Sudokus is

  function Count (s : Sudoset) return Natural is
    res : Natural := 0;
  begin
    for i in Sudigit loop
      if s (i) then
        res := res + 1;
      end if;
    end loop;
    return res;
  end Count;

  function Count_Solved (u : Grid) return Natural is
    total : Natural := 0;
  begin
    for i in Sudigit loop
      for j in Sudigit loop
        if u (i, j).solved then
          total := total + 1;
        end if;
      end loop;
    end loop;
    return total;
  end Count_Solved;

  function Is_Solved (u : Grid) return Boolean is
  begin
    return Count_Solved (u) = 81;
  end Is_Solved;

  procedure Find_Box_Base (i, j : Sudigit; base_i, base_j : out Sudigit) is
    zone_i, zone_j : Natural;
  begin
    zone_i := (i - 1) / 3;
    zone_j := (j - 1) / 3;
    base_i := zone_i * 3 + 1;
    base_j := zone_j * 3 + 1;
  end Find_Box_Base;

  function Is_Valid (u : Grid) return Boolean is
    set, empty : Sudoset;
    v : Sudigit;
    base_i, base_j : Sudigit;
    use HAT;
  begin
    for num in Sudigit loop
      empty (num) := False;
    end loop;
    for i in Sudigit loop
      set := empty;
      --  Check row:
      for j in Sudigit loop
        if u (i, j).solved then
          v := u (i, j).value;
          if set (v) then
            Put_Line (+"Invalid row " & i & "; duplicate value " & v);
            return False;
          end if;
          set (v) := True;
        end if;
      end loop;
    end loop;
    --
    for j in Sudigit loop
      set := empty;
      --  Check column:
      for i in Sudigit loop
        if u (i, j).solved then
          v := u (i, j).value;
          if set (v) then
            Put_Line (+"Invalid column " & j & "; duplicate value " & v);
            return False;
          end if;
          set (v) := True;
        end if;
      end loop;
    end loop;
    --
    for bi in 0 .. 2 loop
      for bj in 0 .. 2 loop
        set := empty;
        base_i := bi * 3 + 1;
        base_j := bj * 3 + 1;
        --  Check box:
        for i in base_i .. base_i + 2 loop
          for j in base_j .. base_j + 2 loop
            if u (i, j).solved then
              v := u (i, j).value;
              if set (v) then
                Put_Line
                  (+"Invalid box " & (1 + bi * 3 + bj) &
                    "; duplicate value " & v);
                return False;
              end if;
              set (v) := True;
            end if;
          end loop;
        end loop;
      end loop;
    end loop;
    return True;
  end Is_Valid;

  procedure Adapt_Sets (u : in out Grid; i, j : Sudigit) is
    num : Sudigit;
    base_i, base_j : Sudigit;
  begin
    if u (i, j).solved then
      num := u (i, j).value;
      for k in Sudigit loop
        --  Disable row:
        u (i, k).set (num) := False;
        --  Disable column:
        u (k, j).set (num) := False;
        Find_Box_Base (i, j, base_i, base_j);
        --  Disable square:
        for ii in base_i .. base_i + 2 loop
          for jj in base_j .. base_j + 2 loop
            u (ii, jj).set (num) := False;
          end loop;
        end loop;
      end loop;
    else
      HAT.Put_Line ("Cell i, j is not solved!");  --  !! Full Ada: raise some exception
    end if;
  end Adapt_Sets;

  procedure Adapt_All_Sets (u : in out Grid) is
  begin
    for i in Sudigit loop
      for j in Sudigit loop
        if u (i, j).solved then
          Adapt_Sets (u, i, j);
        end if;
      end loop;
    end loop;
  end Adapt_All_Sets;

  procedure Mark_Solution (u : in out Grid; i, j, num : Sudigit) is
  begin
    u (i, j).value := num;
    u (i, j).solved := True;
    Adapt_Sets (u, i, j);
    for n in Sudigit loop
      u (i, j).set (n) := False;
    end loop;
  end Mark_Solution;

  ---------------
  --  Singles  --
  ---------------

  procedure Handle_Naked_Single
    (u : in out Grid; i, j : Sudigit; found : in out Natural) is
  begin
    if Count (u (i, j).set) = 1 then
      found := found + 1;
      for num in Sudigit loop
        if u (i, j).set (num) then
          Mark_Solution (u, i, j, num);
          exit;
        end if;
      end loop;
    end if;
  end Handle_Naked_Single;

  procedure Handle_Naked_Singles (u : in out Grid; found : out Natural) is
  begin
    found := 0;
    for i in Sudigit loop
      for j in Sudigit loop
        if not u (i, j).solved then
          Handle_Naked_Single (u, i, j, found);
        end if;
      end loop;
    end loop;
  end Handle_Naked_Singles;

  procedure Handle_Hidden_Single
    (u : in out Grid; i, j : Sudigit; found : in out Natural)
  is
    base_i, base_j : Sudigit;
    ok : Boolean;
  begin
    Find_Box_Base (i, j, base_i, base_j);
    for num in Sudigit loop
      if u (i, j).set (num) then
        --  Check row:
        ok := True;
        for jj in Sudigit loop
          if jj /= j then
            --  Check that the digit is NOT possible elsewhere on the row.
            ok := ok and not u (i, jj).set (num);
            exit when not ok;
          end if;
        end loop;
        if not ok then
          --  Check column:
          ok := True;
          for ii in Sudigit loop
            if ii /= i then
              --  Check that the digit is NOT possible elsewhere on the column.
              ok := ok and not u (ii, j).set (num);
              exit when not ok;
            end if;
          end loop;
        end if;
        if not ok then
          --  Check box:
          ok := True;
          for ii in base_i .. base_i + 2 loop
            for jj in base_j .. base_j + 2 loop
              if ii /= i or else j /= jj then
                --  Check that the digit is NOT possible elsewhere in the box.
                ok := ok and not u (ii, jj).set (num);
                exit when not ok;
              end if;
            end loop;
          end loop;
        end if;
        if ok then
          found := found + 1;
          Mark_Solution (u, i, j, num);
          exit;
        end if;
      end if;
    end loop;
  end Handle_Hidden_Single;

  procedure Handle_Hidden_Singles (u : in out Grid; found : out Natural) is
  begin
    found := 0;
    for i in Sudigit loop
      for j in Sudigit loop
        if not u (i, j).solved then
          Handle_Hidden_Single (u, i, j, found);
        end if;
      end loop;
    end loop;
  end Handle_Hidden_Singles;

  --------------------
  --  Locked cells  --
  --------------------

  procedure Handle_Locked_Cells_Outside_A_Box
    (u : in out Grid; i, j : Sudigit; found : in out Natural)
  is
    base_i, base_j : Sudigit;
    ok_row, ok_col : Boolean;
  begin
    Find_Box_Base (i, j, base_i, base_j);
    for num in Sudigit loop
      if u (i, j).set (num) then
        --  Check if `num` is only on a row / column within its box:
        ok_row := True;
        ok_col := True;
        for ii in base_i .. base_i + 2 loop
          for jj in base_j .. base_j + 2 loop
            if u (ii, jj).set (num) then
              ok_col := ok_col and jj = j;
              ok_row := ok_row and ii = i;
            end if;
          end loop;
        end loop;
        --  We have found that `num` is possible only on a row within the box.
        --  Then, it cannot be on that row outside the box.
        if ok_row then
          for jj in Sudigit loop
            if jj not in base_j .. base_j + 2 and then u (i, jj).set (num) then
              found := found + 1;
              u (i, jj).set (num) := False;
            end if;
          end loop;
        end if;
        --  We have found that `num` is possible only on a column within the box.
        --  Then, it cannot be on that column outside the box.
        if ok_col then
          for ii in Sudigit loop
            if ii not in base_i .. base_i + 2 and then u (ii, j).set (num)  then
              found := found + 1;
              u (ii, j).set (num) := False;
            end if;
          end loop;
        end if;
      end if;
    end loop;
  end Handle_Locked_Cells_Outside_A_Box;

  procedure Handle_Locked_Cells_Outside_Boxes (u : in out Grid; found : out Natural) is
  begin
    found := 0;
    for i in Sudigit loop
      for j in Sudigit loop
        if not u (i, j).solved then
          Handle_Locked_Cells_Outside_A_Box (u, i, j, found);
        end if;
      end loop;
    end loop;
  end Handle_Locked_Cells_Outside_Boxes;

  procedure Handle_Locked_Cells_Inside_A_Box
    (u : in out Grid; i, j : Sudigit; found : in out Natural)
  is
    base_i, base_j : Sudigit;
    ok_row, ok_col : Boolean;
  begin
    Find_Box_Base (i, j, base_i, base_j);
    for num in Sudigit loop
      if u (i, j).set (num) then
        --  Check if `num` is only on a row / column within its box:
        ok_row := True;
        for jj in Sudigit loop
          if jj not in base_j .. base_j + 2 then
            --  Row i, but outside the box
            if u (i, jj).set (num) then
              ok_row := False;
              exit;
            end if;
          end if;
        end loop;
        ok_col := True;
        for ii in Sudigit loop
          if ii not in base_i .. base_i + 2 then
            --  Column j, but outside the box
            if u (ii, j).set (num) then
              ok_col := False;
              exit;
            end if;
          end if;
        end loop;
        for ii in base_i .. base_i + 2 loop
          for jj in base_j .. base_j + 2 loop
            --  We have found that on the whole row, `num` is possible only within the box.
            --  Then, it cannot be on another row inside the box.
            if ok_row and then i /= ii and then u (ii, jj).set (num) then
              found := found + 1;
              u (ii, jj).set (num) := False;
            end if;
            --  We have found that on the whole column, `num` is possible only within the box.
            --  Then, it cannot be on another column inside the box.
            if ok_col and then j /= jj and then u (ii, jj).set (num) then
              found := found + 1;
              u (ii, jj).set (num) := False;
            end if;
          end loop;
        end loop;
      end if;
    end loop;
  end Handle_Locked_Cells_Inside_A_Box;

  procedure Handle_Locked_Cells_Inside_Boxes (u : in out Grid; found : out Natural) is
  begin
    found := 0;
    for i in Sudigit loop
      for j in Sudigit loop
        if not u (i, j).solved then
          Handle_Locked_Cells_Inside_A_Box (u, i, j, found);
        end if;
      end loop;
    end loop;
  end Handle_Locked_Cells_Inside_Boxes;

  procedure Show (u : Grid; title : HAT.VString) is
    min_poss : Natural := Natural'Last;
    poss : Natural;
    use HAT;
  begin
    Put_Line (title);
    Put_Line (Length (title) * '=');
    New_Line;
    Put_Line (+"Grid, " & Count_Solved (u) & " digits set         Possibilities");
    New_Line;
    for i in Sudigit loop
      for j in Sudigit loop
        if j mod 3 = 1 then
          Put ('|');
        else
          Put (' ');
        end if;
        if u (i, j).solved then
          Put (u (i, j).value, 0);
        else
          Put ('_');
        end if;
      end loop;
      Put ("|         ");
      for j in Sudigit loop
        if j mod 3 = 1 then
          Put ('|');
        else
          Put (' ');
        end if;
        if u (i, j).solved then
          Put ('_');
        else
          poss := Count (u (i, j).set);
          Put (poss, 0);
          min_poss := Min (min_poss, poss);
        end if;
      end loop;
      Put_Line ('|');
      if i = 3 or else i = 6 then
        Put_Line (":-----+-----+-----:         :-----+-----+-----:");
      end if;
    end loop;
    New_Line;
    if min_poss in Sudigit then
      Put_Line (+"                            Minimum: " & min_poss);
    end if;
  end Show;

  procedure Show_Detailed_Possibilities (u : Grid) is
    use HAT;
  begin
    for i in Sudigit loop
      for j in Sudigit loop
        for num in Sudigit loop
          if u (i, j).set (num) then
            Put (num, 0);
          else
            Put ('.');
          end if;
        end loop;
        if j mod 3 = 0 then
          Put ('|');
        else
          Put (' ');
        end if;
      end loop;
      New_Line;
      if i mod 3 = 0 then
        Put_Line (90 * '-');
      end if;
    end loop;
  end Show_Detailed_Possibilities;

  procedure Convert_Data (s : in Sudostrings; pack : in out Sudopack) is

    use HAT;

    procedure Convert_String (i : Sudigit; s : VString) is
      c : Character;
    begin
      for j in Sudigit loop
        c := Element (s, j);
        case c is
          when ' ' =>
            pack.u (i, j).solved := False;
            pack.u (i, j).set := pack.full;
          when '1' .. '9' =>
            pack.u (i, j).solved := True;
            pack.u (i, j).set := pack.empty;
            pack.u (i, j).value := Ord (c) - Ord ('0');
          when others =>
            Put_Line ("Data Error!");  --  !! Full Ada: raise some exception
        end case;
      end loop;
    end Convert_String;

  begin
    Initialize (pack);
    for i in Sudigit loop
      Convert_String (i, s (i));
    end loop;
  end Convert_Data;

  procedure Initialize (pack : out Sudopack) is
  begin
    for num in Sudigit loop
      pack.full (num) := True;
      pack.empty (num) := False;
    end loop;
  end Initialize;

end Sudokus;
