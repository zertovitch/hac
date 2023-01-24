with HAT;

procedure Sudoku is

  subtype Sudigit is Integer range 1 .. 9;

  use HAT;

  type Sudostrings is array (Sudigit) of VString;

  procedure Solve (s : Sudostrings; name : VString) is

    type Sudoset is array (Sudigit) of Boolean;

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

    full : Sudoset;

    type Cell is record
      solved : Boolean;
      value  : Sudigit;
      set    : Sudoset;
    end record;

    type Grid is array (Sudigit, Sudigit) of Cell;

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

    procedure Show (u : Grid; title : VString) is
      min_poss : Natural := Natural'Last;
      poss : Natural;
    begin
      Put_Line (title);
      Put_Line (Length (title) * '=');
      New_Line;
      Put_Line (+"Grid, " & Count_Solved (u) & " digits set     Possibilities");
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
        Put ("|     ");
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
          Put_Line ("-------------------     -------------------");
        end if;
      end loop;
      New_Line;
      if min_poss in Sudigit then
        Put_Line (+"                        Minimum: " & min_poss);
      end if;
    end Show;

    procedure Convert_Data (data : out Grid) is

      procedure Convert_String (i : Sudigit; s : VString) is
        c : Character;
      begin
        for j in Sudigit loop
          c := Element (s, j);
          case c is
            when ' ' =>
              data (i, j).solved := False;
              data (i, j).set := full;
            when '1' .. '9' =>
              data (i, j).solved := True;
              data (i, j).value  := Ord (c) - Ord ('0');
            when others =>
              Put_Line ("Data Error!");  --  !! Full Ada: raise some exception
          end case;
        end loop;
      end Convert_String;

    begin
      for i in Sudigit loop
        Convert_String (i, s (i));
      end loop;
    end Convert_Data;

    procedure Adapt_Sets (u : in out Grid; i, j : Sudigit) is
      num : Sudigit;
      zone_i, zone_j : Natural;
      base_i, base_j : Sudigit;
    begin
      if u (i, j).solved then
        num := u (i, j).value;
        for k in Sudigit loop
          --  Disable row:
          u (i, k).set (num) := False;
          --  Disable column:
          u (k, j).set (num) := False;
          zone_i := (i - 1) / 3;
          zone_j := (j - 1) / 3;
          base_i := zone_i * 3 + 1;
          base_j := zone_j * 3 + 1;
          --  Disable square:
          for ii in base_i .. base_i + 2 loop
            for jj in base_j .. base_j + 2 loop
              u (ii, jj).set (num) := False;
            end loop;
          end loop;
        end loop;
      else
        Put_Line ("Solved Error!");  --  !! Full Ada: raise some exception
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

    procedure Naked_Single (u : in out Grid; i, j : Sudigit; found : in out Natural) is
    begin
      if not u (i, j).solved and then Count (u (i, j).set) = 1 then
        found := found + 1;
        for k in Sudigit loop
          if u (i, j).set (k) then
            u (i, j).value := k;
            u (i, j).solved := True;
            Adapt_Sets (u, i, j);
            exit;
          end if;
        end loop;
      end if;
    end Naked_Single;

    procedure Naked_Singles (u : in out Grid; found : out Natural) is
    begin
      found := 0;
      for i in Sudigit loop
        for j in Sudigit loop
          Naked_Single (u, i, j, found);
        end loop;
      end loop;
    end Naked_Singles;

    u : Grid;
    ns_found : Natural;

  begin
    for i in Sudigit loop
      full (i) := True;
    end loop;
    Convert_Data (u);
    Adapt_All_Sets (u);
    Show (u, "Initial board for: " & name);

    for round in 1 .. 81 loop
      Naked_Singles (u, ns_found);
      Show (u, +"Round " & round);
      Put_Line (+"Found: " & ns_found & " naked singles");
      exit when Is_Solved (u);
      New_Line;
    end loop;

  end Solve;

  easy : Sudostrings;
begin
  easy (1) := +"1   83 57";
  easy (2) := +"9 27     ";
  easy (3) := +"  5   34 ";
  easy (4) := +"5   7  8 ";
  easy (5) := +"4  9 1  5";
  easy (6) := +" 1  5   6";
  easy (7) := +" 57   8  ";
  easy (8) := +"     87 4";
  easy (9) := +"24 16   9";

  Solve (easy, +"Easy");
end Sudoku;
