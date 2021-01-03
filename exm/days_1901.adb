--  Prototype developed using HAC [1] and LEA [2], for portable date output of Excel Writer [3].
--
--  [1]: http://hacadacompiler.sf.net/
--  [2]: http://l-e-a.sf.net/
--  [3]: http://excel-writer.sf.net/

with HAC_Pack; use HAC_Pack;

procedure Days_1901 is

  --  1901 is the lowest year supported by Ada.Calendar.
  --  1900 is not a leap year, but Lotus 1-2-3 then Excel consider it as a leap year.
  --  So, with 1901, we skip that issue anyway...
  --
  function Days_since_1901 (y, m, d : Integer) return Integer is
    function Is_leap (y : Integer) return Boolean is
    begin
      if y mod 4 = 0 then
        if y mod 100 = 0 then
          if y mod 400 = 0 then
            return True;
          else
            return False;
          end if;
        else
          return True;
        end if;
      else
        return False;
      end if;
    end Is_leap;
    days_of_previous_months : Integer;
    days_of_previous_years  : Integer;
    y_diff, y_diff_4, y_diff_100, y_diff_400 : Integer;
  begin
    case m is
      when 02 => days_of_previous_months := 31;
      when 03 => days_of_previous_months := 59;
      when 04 => days_of_previous_months := 90;
      when 05 => days_of_previous_months := 120;
      when 06 => days_of_previous_months := 151;
      when 07 => days_of_previous_months := 181;
      when 08 => days_of_previous_months := 212;
      when 09 => days_of_previous_months := 243;
      when 10 => days_of_previous_months := 273;
      when 11 => days_of_previous_months := 304;
      when 12 => days_of_previous_months := 334;
      when others => days_of_previous_months := 0;
    end case;
    if (m > 2) and Is_leap (y) then  --  February has 29 days in leap years.
      days_of_previous_months := days_of_previous_months + 1;
    end if;
    --
    y_diff     := (y - 1)       - 1900;
    y_diff_4   := (y - 1) / 4   - 1900 / 4;
    y_diff_100 := (y - 1) / 100 - 1900 / 100;
    y_diff_400 := (y - 1) / 400 - 1900 / 400;
    --  Add leap years from 1901 (included) to now (excluded).
    days_of_previous_years := 365 * y_diff + y_diff_4 - y_diff_100 + y_diff_400;
    --
    return days_of_previous_years + days_of_previous_months + d - 1;
  end Days_since_1901;
  --
begin
  for y in 1901 .. 2200 loop
    Put ("Days from 1/1/1901 to : ... 12/31/");
    Put (y);
    Put_Line (Days_since_1901 (y, 12, 31));
  end loop;
end Days_1901;
