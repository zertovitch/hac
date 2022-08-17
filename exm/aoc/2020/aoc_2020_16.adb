--  Solution to Advent of Code 2020, Day 16
-------------------------------------------
--  Ticket Translation
--
--  https://adventofcode.com/2020/day/16
--
--  Run-time with HAC:
--    *  1.08  seconds for a Celeron N3350 @ max 1.1 GHz
--    *  0.35  seconds for a i5-9400 @ 2.9 GHz
--  Run-time with GNAT, AoC_Build_Mode = "Fast":
--    *  0.009 seconds for a i5-9400 @ 2.9 GHz
--
--  HAC 0.084 "nice to have"'s detected in this exercise:
--
--    *     ` cc_match := (others => (others => True)); `
--
with HAT;
--  ^ For a build with "full Ada": files HAT*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .

with Interfaces;  --  Needed for GNAT (Integer_64).

procedure AoC_2020_16 is
  use HAT, Interfaces;

  criteria : constant := 20;
  subtype Criteria_Range is Positive range 1 .. criteria;
  val_11, val_12, val_21, val_22 : array (Criteria_Range) of Integer;
  --
  function Is_Valid (value : Integer; c : Criteria_Range) return Boolean is
  begin
    return value in val_11 (c) .. val_12 (c) or else
           value in val_21 (c) .. val_22 (c);
  end Is_Valid;
  --
  --  In this problem we have as many columns as criteria:
  subtype Column_Range is Criteria_Range;
  ticket, my_ticket : array (Column_Range) of Integer;
  --
  or_str : String (1 .. 4);  --  " or ";
  ok, possible_row, single_match_only : Boolean;
  cc_match : array (Column_Range, Criteria_Range) of Boolean;
  unlinked : array (Column_Range) of Boolean;
  cc, matching : Integer;
  err : Natural := 0;
  prod : Integer_64;
  c, sep1, sep2, sep3 : Character;
  f : File_Type;
  compiler_test_mode : constant Boolean := Argument_Count >= 2;
  verbose : constant := 0;
begin
  Open (f, "aoc_2020_16.txt");
  for crit in 1 .. criteria loop
    loop
      Get (f, c);
      exit when c = ':';
    end loop;
    Get (f, val_11 (crit));
    Get (f, sep1);
    Get (f, val_12 (crit));
    Get (f, or_str);
    Get (f, val_21 (crit));
    Get (f, sep2);
    Get (f, val_22 (crit));
    if verbose > 0 then
      Put (val_11 (crit), 0); Put (" - "); Put (val_12 (crit), 0);
      Put (or_str);
      Put (val_21 (crit), 0); Put (" - "); Put (val_22 (crit), 0);
      New_Line;
    end if;
  end loop;
  Skip_Line (f, 3);
  --
  --  Read my ticket.
  --
  if verbose > 0 then Put_Line ("my ticket:"); end if;
  for col in Column_Range loop
    Get (f, my_ticket (col));
    if col < criteria then Get (f, sep3); end if;
    if verbose > 0 then
      Put (my_ticket (col), 0); Put (' ');
    end if;
  end loop;
  Skip_Line (f, 3);
  if verbose > 0 then New_Line; end if;
  --
  for col in Column_Range loop
    unlinked (col) := True;
    for crit in Criteria_Range loop
      cc_match (col, crit) := True;
    end loop;
  end loop;
  --
  --  Read the nearby tickets.
  --
  while not End_Of_File (f) loop
    possible_row := True;
    for col in Column_Range loop
      Get (f, ticket (col));
      if col < criteria then Get (f, sep3); end if;
      if verbose > 1 then Put (ticket (col)); end if;
      ok := False;
      for crit in Criteria_Range loop
        if Is_Valid (ticket (col), crit) then
          ok := True;
          exit;
        end if;
      end loop;
      if not ok then
        if verbose > 0 then
          Put ("Invalid value: "); Put (ticket (col), 0); New_Line;
        end if;
        err := err + ticket (col);
        possible_row := False;
      end if;
    end loop;
    if possible_row then
      --  Each column in this row fits one or the other criterium.
      --  We switch off possible combinations that don't match for this data row.
      for col in Column_Range loop
        for crit in Criteria_Range loop
          if not Is_Valid (ticket (col), crit) then
            --  This column is invalid for this criterium:
            cc_match (col, crit) := False;
          end if;
        end loop;
      end loop;
    end if;
    if verbose > 1 then New_Line; end if;
  end loop;
  Close (f);
  --
  --  Part 2 only: for each column, find THE matching criterium.
  --
  for round in Column_Range loop
    single_match_only := True;
    --  Find a column that fits only one criterium.
    --  (Q: is this problem symmetric? Can we find a
    --      criterium matching only one column?)
    for col in Column_Range loop
      if unlinked (col) then
        cc := 0;
        for crit in Criteria_Range loop
          if cc_match (col, crit) then
            cc := cc + 1;
            matching := crit;
            if cc > 1 then
              single_match_only := False;
              exit;
            end if;
          end if;
        end loop;
        if cc = 1 then  --  Column col is valid for only one criterium.
          if verbose > 0 then
            Put ("  Column "); Put (col, 0);
            Put (" fits only criterium: "); Put (matching, 0);
            New_Line;
          end if;
          unlinked (col) := False;
          for other_col in Column_Range loop
            if other_col /= col then
              --  Cancel the matching criterium for other columns.
              cc_match (other_col, matching) := False;
            end if;
          end loop;
        end if;
      end if;
    end loop;
    if verbose > 0 then
      Put ("Round "); Put (round, 0); Put_Line (" done.");
    end if;
    exit when single_match_only;
  end loop;
  --
  --  Columns for my ticket, for criteria "destination" (1 to 6).
  --
  prod := 1;
  for crit in Criteria_Range loop
    if verbose > 0 then
      Put ("Criterium: "); Put (crit, 0);
    end if;
    for col in Column_Range loop
      --  Find THE matching criterium.
      if cc_match (col, crit) then
        if verbose > 0 then
          Put (" column: "); Put (col, 0);
        end if;
        prod := prod * Integer_64 (my_ticket (col));
      end if;
    end loop;
    if verbose > 0 then
      New_Line;
    end if;
    exit when crit = 6;
  end loop;
  if compiler_test_mode then
    if err /= Integer_Value (Argument (1)) or
       prod /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put ("Part 1: ticket scanning error rate . . . . . . . .  : ");
    Put (err, 0);   --  Validated by AoC: 23954
    New_Line;
    Put ("Part 2: product of column numbers for ""destination"" :");
    Put (Integer_64'Image (prod));  --  Validated by AoC: 453459307723
    New_Line;
  end if;
end AoC_2020_16;
