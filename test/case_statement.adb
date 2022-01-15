with HAL; use HAL;

procedure Case_Statement is

  procedure Failure (Msg : VString) is
  begin
    Put_Line (+"Failure in test: [" & Msg & ']');
    Set_Exit_Status (1);  --  Compiler test failed.
  end Failure;

  procedure Assert (Msg : VString; Check : in Boolean) is
  --  Similar to RM 11.4.2 but without raising an exception.
  begin
    if not Check then Failure (Msg & ", assertion"); end if;
  end Assert;

  after_int_case : Boolean := False;

  procedure Test_Int (i : Integer) is
  begin
    case (2 * (i + 1)) / 2 - 1 is
      when 1 | -1 =>
        if abs i /= 1 then
          Put_Line ("Compiler bug [Int, A]");
          Set_Exit_Status (1);  --  Compiler test failed.
        end if;
      when -7 =>
        if i + 7 /= 0 then
          Put_Line ("Compiler bug [Int, B]");
          Set_Exit_Status (1);  --  Compiler test failed.
        end if;
      when others => null;
      --  !! When "OTHERS" omitted: HAC compiles but the VM enters a Case_Check_Error state.
      --
      --  when 9 | others => null;  --  the choice "others" must appear alone and last
      --  when 2 => null;  --  the choice "others" must appear alone and last
    end case;
    after_int_case := True;
  end Test_Int;

  some_vowels_occurrences : Integer := 0;
  small_consonants_occurrences : Integer := 0;

  procedure Test_Char (c : Character) is
  begin
    case c is
      when 'a' | 'A' =>
        if (c /= 'a') and (c /= 'A') then
          Put (c); Put_Line ("  Compiler bug [Char, A]");
          Set_Exit_Status (1);  --  Compiler test failed.
        end if;
        some_vowels_occurrences := some_vowels_occurrences + 1;
      when '*' =>
        null;
      when 'e' =>
        some_vowels_occurrences := some_vowels_occurrences + 1;
      when 'b' .. 'd' | 'f' .. 'h' | 'j' .. 'n' | 'p' .. 't' | 'v' .. 'x' | 'z' =>
        small_consonants_occurrences := small_consonants_occurrences + 1;
      when others => null;
    end case;
  end Test_Char;

  big_A : constant Character := 'A';  --  8-bit character

begin
  for i in -10 .. 10 loop
    Test_Int (i);
  end loop;
  if not after_int_case then
    Put_Line ("Compiler bug [Int, Z]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
  --
  for c in big_A .. 'z' loop
    Test_Char (c);
  end loop;
  Assert (+"Char, CASE 1", some_vowels_occurrences = 3);
  Assert (+"Char, CASE 2", small_consonants_occurrences = 20);
end Case_Statement;
