with HAT;
with Testing_Utilities;

procedure Case_Statement is
  use HAT, Testing_Utilities;

  after_int_case : Boolean := False;

  procedure Test_Int (i : Integer) is
  begin
    case (2 * (i + 1)) / 2 - 1 is
      when 1 | -1 =>
        Assert (abs i = 1, +"Compiler bug [CASE test, Int, A]");
      when -7 =>
        Assert (i + 7 = 0, +"Compiler bug [CASE test, Int, B]");
      when others => null;
      --
      --  when 9 | others => null;  --  the choice "others" must appear alone and last
      --  when 2 => null;  --  the choice "others" must appear alone and last
      --  when 2 .. 9223372036854775807   => null;
      --  when -9223372036854775807 .. -8 => null;  --  Integer'First as literal not supported (overflow)
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
  Assert (after_int_case, +"Compiler bug [CASE, Int, Z]");
  --
  for c in big_A .. 'z' loop
    Test_Char (c);
  end loop;
  Assert (some_vowels_occurrences = 3,       +"Char, CASE 1");
  Assert (small_consonants_occurrences = 20, +"Char, CASE 2");
end Case_Statement;
