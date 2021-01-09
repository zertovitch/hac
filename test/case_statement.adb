with HAL; use HAL;

procedure Case_Statement is

  after_int_case : Boolean := False;

  procedure Test_Int (i : Integer) is
  begin
    case (2*(i + 1)) / 2 - 1 is
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

  vowel_occurences : Integer := 0;

  procedure Test_Char (c: Character) is
  begin
    case c is
      when 'a' | 'A' =>
        if (c /= 'a') and (c /= 'A') then
          Put(c); Put_Line ("  Compiler bug [Char, A]");
          Set_Exit_Status (1);  --  Compiler test failed.
        end if;
        vowel_occurences := vowel_occurences + 1;
      when 'b' | 'B' =>
        null;
      when 'e' =>
        vowel_occurences := vowel_occurences + 1;
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
  if vowel_occurences /= 3 then
    Put_Line ("Compiler bug [Char, Z]");
    Set_Exit_Status (1);  --  Compiler test failed.
  end if;
end Case_Statement;
