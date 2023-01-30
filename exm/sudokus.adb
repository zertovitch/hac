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

  procedure Handle_Multiples_Single_Cell
    (u     : in out Grid;
     i, j  :        Sudigit;
     h     : in out Sudo_Help;
     found : in out Natural)
  is
  begin
    null;
  end Handle_Multiples_Single_Cell;
  
  procedure Handle_Multiples
    (u     : in out Grid;
     h     : in out Sudo_Help;
     found :    out Natural)
  is
  begin
    found := 0;
    for i in Sudigit loop
      for j in Sudigit loop
        if not u (i, j).solved then
          Handle_Multiples_Single_Cell (u, i, j, h, found);
        end if;
      end loop;
    end loop;
  end Handle_Multiples;

  function Technique_Image (t : Resolution_Technique) return HAT.VString is
    use HAT;
  begin
    case t is
      when naked_single            => return +"naked single(s)";
      when hidden_single           => return +"hidden single(s)";
      when locked_cell_outside_box => return +"locked cell(s), outside boxe(s)";
      when locked_cell_inside_box  => return +"locked cell(s), inside boxe(s)";
    end case;
  end Technique_Image;

  procedure Zero (count : out Technique_Count) is
  begin
    for t in Resolution_Technique loop
      count (t) := 0;
    end loop;
  end Zero;

  function Sum (count : Technique_Count) return Natural is
    s : Natural := 0;
  begin
    for t in Resolution_Technique loop
      s := s + count (t);
    end loop;
    return s;
  end Sum;

  procedure Resolution_Round
    (pack            : in out Sudo_Pack;
     help            : in out Sudo_Help;
     title           : in     HAT.VString;
     verbosity_level : in     Natural)
  is
    found : Technique_Count;

    procedure Handle_Techniques is
    begin
      Handle_Naked_Singles (pack.u, found (naked_single));
      if found (naked_single) > 0 then
        return;
      end if;
      --  We search more complicated possibilities,
      --  only when none for the less complicated was found.
      Handle_Hidden_Singles (pack.u, found (hidden_single));
      if found (hidden_single) > 0 then
        return;
      end if;
      Handle_Locked_Cells_Outside_Boxes (pack.u, found (locked_cell_outside_box));
      if found (locked_cell_outside_box) > 0 then
        return;
      end if;
      Handle_Locked_Cells_Inside_Boxes (pack.u, found (locked_cell_inside_box));
    end Handle_Techniques;

    use HAT;

  begin
    Zero (found);
    Handle_Techniques;
    pack.stalling := Sum (found) = 0;
    if verbosity_level > 1 then
      if pack.stalling then
        Put_Line ("Stalling. Search abandoned.");
        if verbosity_level > 2 then
          Show_Detailed_Possibilities (pack.u);
        end if;
      end if;
      if verbosity_level > 2 or else Is_Solved (pack.u) then
        Show (pack.u, title);
      end if;
      if verbosity_level > 2 then
        for t in Resolution_Technique loop
          if found (t) > 0 then
            Put_Line (+"Found: " & found (t) & ' ' & Technique_Image (t));
          end if;
        end loop;
        New_Line;
      end if;
    end if;
    for t in Resolution_Technique loop
      pack.total (t) := pack.total (t) + found (t);
    end loop;
  end Resolution_Round;

  procedure Solve
    (pack            : in out Sudo_Pack;
     help            : in out Sudo_Help;
     name            : in     HAT.VString;
     verbosity_level : in     Natural)
  is
    use HAT;
  begin
    Adapt_All_Sets (pack.u);
    case verbosity_level is
      when 0 =>
        null;  --  Completely silent, except for errors.
      when 1 =>
        Put ("Puzzle: " & name & " - ");
      when others =>
        New_Line;
        Show (pack.u, ">>>>> Initial board for: " & name);
    end case;
    if not Is_Valid (pack.u) then
      Put_Line ("Initial board is invalid!");
      return;
    end if;
    --
    for round in 1 .. Sudigit'Last * Sudigit'Last loop
      Resolution_Round
        (pack, help, +"After round " & round & " for " & name, verbosity_level);
      exit when pack.stalling or else Is_Solved (pack.u);
    end loop;
    --
    if verbosity_level > 2 then
      Put_Line ("Totals of techniques used:");
      for t in Resolution_Technique loop
        if pack.total (t) > 0 then
          Put_Line (+"   found: " & pack.total (t) & ' ' & Technique_Image (t));
        end if;
      end loop;
      New_Line;
    end if;
    if not Is_Valid (pack.u) then
      Put_Line ("Solution or current state is invalid!");
      return;
    end if;
    if verbosity_level = 1 then
      if pack.stalling then
        Put ("** stalling **, algorithm not smart enough!");
      elsif Is_Solved (pack.u) then
        Put ("solved and checked.");
      else
        Put ("Neither solved nor unsolved ?!");
      end if;
      Put_Line (" Increase verbosity_level for details.");
    end if;
  end Solve;

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

  procedure Convert_Data
    (s    : in     Sudo_Strings;
     pack : in out Sudo_Pack;
     help : in out Sudo_Help)
  is
    procedure Convert_String (i : Sudigit; s : HAT.VString) is
      c : Character;
      use HAT;
    begin
      for j in Sudigit loop
        c := Element (s, j);
        case c is
          when ' ' =>
            pack.u (i, j).solved := False;
            pack.u (i, j).set := help.full;
          when '1' .. '9' =>
            pack.u (i, j).solved := True;
            pack.u (i, j).set := help.empty;
            pack.u (i, j).value := Ord (c) - Ord ('0');
          when others =>
            Put_Line ("Data Error!");  --  !! Full Ada: raise some exception
        end case;
      end loop;
    end Convert_String;

  begin
    Initialize (pack);
    Initialize_Helper (help);
    for i in Sudigit loop
      Convert_String (i, s (i));
    end loop;
  end Convert_Data;

  procedure Initialize (pack : out Sudo_Pack) is
  begin
    Zero (pack.total);
    pack.stalling := False;
  end Initialize;

  procedure Initialize_Helper (help : out Sudo_Help) is
    i_shifted, bits_1 : Natural;
    comb_index : Positive;
  begin
    for num in Sudigit loop
      help.full (num) := True;
      help.empty (num) := False;
      help.max_combi (num) := 0;
    end loop;
    --
    for i in 1 .. 510 loop
      i_shifted := i;
      bits_1 := 0;
      for d in Sudigit loop
        if i_shifted mod 2 = 1 then
          bits_1 := bits_1 + 1;
        end if;
        i_shifted := i_shifted / 2;
      end loop;
      --  For bits_1 = 3 (say), we have found a new
      --  combination with 3 digits which correspond to
      --  then position of the bit in the binary
      --  representation of i.
      help.max_combi (bits_1) := help.max_combi (bits_1) + 1;
      i_shifted := i;
      comb_index := 1;
      for d in Sudigit loop
        if i_shifted mod 2 = 1 then
          help.table (bits_1, help.max_combi (bits_1))(comb_index) := d;
          comb_index := comb_index + 1;
        end if;
        i_shifted := i_shifted / 2;
      end loop;
    end loop;
    --  Some table outputs:
    --
    --  HAT.Put_Line ("Combinations:");
    --  for d in 1 .. 8 loop
    --    HAT.Put ("For");
    --    HAT.Put (d, 2);
    --    HAT.Put_Line (help.max_combi (d));  --  = Binomial (9, d)
    --  end loop;
    --  HAT.Put_Line ("All doubles:");
    --  for i in 1 .. 36 loop
    --    for j in 1 .. 2 loop
    --      HAT.Put (help.table (2, i)(j), 2);
    --    end loop;
    --    HAT.New_Line;
    --  end loop;
    --  HAT.Put_Line ("All triples:");
    --  for i in 1 .. 84 loop
    --    for j in 1 .. 3 loop
    --      HAT.Put (help.table (3, i)(j), 2);
    --    end loop;
    --    HAT.New_Line;
    --  end loop;
  end Initialize_Helper;

end Sudokus;
