with HAT; use HAT;

procedure Strings_demo is
  s1, s2, s4, s4_s4 : VString;
  --
  n : constant := 10;
  type Str_Vector is array (1 .. n) of VString;
  type Bi_Vector is record
    A, B : Str_Vector;
  end record;
  --
  procedure A_to_B (BV : in out Bi_Vector) is
  begin
    BV.B := BV.A;
  end A_to_B;
  --
  procedure Reverso (SV : in out Str_Vector) is
    SV2 : Str_Vector;
  begin
    for i in 1 .. n loop
      SV2 (n - i + 1) := SV (i);
    end loop;
    SV := SV2;
  end Reverso;
  --
  procedure Show (SV : in Str_Vector) is
  begin
    for i in 1 .. n loop
      Put_Line (SV (i));
    end loop;
  end Show;
  --
  procedure Slice_Show (v : VString) is
    l : constant Integer := Length (v);
    c : Character;
    row : VString;
  begin
    for i in reverse 1 .. l loop
      Put_Line (Slice (v, 1, i));
    end loop;
    --
    for i in 1 .. l loop
      Put ((i - 1) * ' ');
      Put_Line (Slice (v, i, l));
    end loop;
    --
    for i in 1 .. l loop
      row := +"";
      for j in 1 .. l loop
        if i = j then
          c := Element (v, i);
        elsif abs (i - j) = 1 then
          c := ' ';
        else
          c := '_';
        end if;
        row := row & c;
      end loop;
      Put_Line (row);
    end loop;
  end Slice_Show;
  --
  procedure Up_Low (v : VString) is
    row : VString;
    c : Character;
  begin
    Put_Line (v);
    New_Line;
    Put_Line (To_Upper (v));
    Put_Line (To_Lower (v));
    New_Line;
    for i in 1 .. Length (v) loop
      row := +"";
      for j in 1 .. Length (v) loop
        c := Element (v, j);
        if i = j then
          row := row & To_Upper (c);
        else
          row := row & To_Lower (c);
        end if;
      end loop;
      Put_Line (row);
    end loop;
  end Up_Low;

  s3 : constant VString := +" world";
  ZZ : Bi_Vector;
  padded : VString;
  Str3 : constant String (6 .. 8) := "But";
  v_char : VString;
begin
  s2 := +"Hello";             --  Convert from literal string, copy to s2.
  v_char := +'.';             --  Convert from Character
  s1 := s2;                   --  Copy VString to VString
  s4 := s1 & s3;              --  Concatenation VString & VString
  Put_Line (5 * (s1 & ' '));  --  Multiplication, and Concatenation with Character
  for i in 1 .. 4 loop Put (v_char & s1); end loop; New_Line;
  padded := +"   " & '"' & s1 & """    ";
  New_Line;
  Put_Line ("->" & padded & "<- original");
  Put_Line ("->" & Trim_Left (padded)  & "<-  Trim_Left");
  Put_Line ("->" & Trim_Right (padded) & "<-  Trim_Right");
  Put_Line ("->" & Trim_Both (padded)  & "<-  Trim_Both");
  s4 := "---> """ & s4 & '"';
  Put_Line (s4);
  Put_Line (">> " & s4 & ' ' & '!' & " <<");
  s4 := +"abc" & 'd' & "ef";
  --
  for i in 1 .. n loop
    ZZ.A (i) := +"";
    for j in 1 .. n loop
      if j = i then
        ZZ.A (i) := ZZ.A (i) & '*';
      else
        ZZ.A (i) := ZZ.A (i) & '.';
      end if;
    end loop;
  end loop;
  --
  A_to_B (ZZ);
  --
  Show (ZZ.B);
  Reverso (ZZ.B);
  Show (ZZ.B);
  --
  Slice_Show (+"What's happening to this string?!");
  Up_Low (+"Upside Down");
  --
  Put_Line (+"  [" & (-123) & "]  [" & 123 & "]  [" & Real (Pi) & ']');
  Put_Line (123 & (+" & Right VString"));
  Put_Line (Real (-456.0) & (+" & Right VString"));
  Put_Line (Real (-456.0e9) & (+" & Right VString"));
  Put_Line (Real (456.0e11) & (+" & Right VString"));
  Put_Line (Real (456.0e66) & (+" & Right VString"));
  Put_Line (Real (456.789e13) & (+" & Right VString"));
  Put_Line (Real (4.56789e13) & (+" & Right VString"));
  Put_Line (Real (4.56789e14) & (+" & Right VString"));
  Put_Line (Real (4.56789e15) & (+" & Right VString"));
  Put_Line (Real (1.79769313486232E+307) & (+" & Right VString"));
  Put_Line ("Integer: Image...        " & Image (456789));
  Put_Line ("Real: Image...           " & Image (Real (456789.0)));
  Put_Line ("Real: Image...           " & Image (Real (4.56789e10)));
  Put_Line ("Real: Image attribute..."  & Real'Image (4.56789e10));
  Put_Line (Float_Value   (+"456.789e13"));  --  Ada.Text_IO display of the Real number
  Put_Line (Integer_Value (+"456"));         --  Ada.Text_IO display of the Integer number
  Put (Str3); Put ("! ");
  s1 := +Str3;
  Put_Line (s1 & "!!");
  Put ("Heads: "); for i in 1 .. 10 loop Put (Head (+"Head...", i)); end loop;
  New_Line;
  Put ("Tails: "); for i in 1 .. 10 loop Put (Tail (+"...Tail", i)); end loop;
  New_Line (2);
  Put_Line ("----------- Strings_demo: That's all folks, the show is over! -----------");
  New_Line (3);
  Put_Line ('.');
  --
  --  Quick tests. More systematic tests can be found in:  test/strings.adb
  --
  if s2 /= To_VString ("Hello") then
    --  `To_VString ("Hello")` and `+"Hello"` are identical function calls.
    Put ("Ooops?");
  end if;
  if +Str3 /= To_VString (Str3) then
    --  `To_VString (s3)` and `+s3` are identical function calls.
    Put ("Ooops?");
  end if;
  if +'x' /= To_VString ('x') then
    --  `To_VString ('x')` and `+'x'` are identical function calls.
    Put ("Ooops?");
  end if;
  if s4 /= +"abcdef" then  --  Comparison VString to VString
    Put ("Ooops?");
  end if;
  if s4 /=  "abcdef" then  --  Comparison VString to String_Literal
    Put ("Ooops?");
  end if;
  if Length (s4) /= 6 then
    Put ("Ooops?");
  end if;
  s4_s4 := s4 & s4;  --  abcdefabcdef
                     --  123456789012
  if Index (s4_s4, +"cd") /= 3 or
     Index (s4_s4,  "cd") /= 3 or
     Index (s4_s4,  'c')  /= 3
  then
    Put ("[Index] Ooops?");
  end if;
  if Index (s4_s4, +"cd", 4) /= 9 or
     Index (s4_s4,  "cd", 4) /= 9 or
     Index (s4_s4,  'c', 4)  /= 9
  then
    Put ("[Index, From] Ooops?");
  end if;
  if Index_Backward (s4_s4, +"cd") /= 9 or
     Index_Backward (s4_s4,  "cd") /= 9 or
     Index_Backward (s4_s4,  'c')  /= 9
  then
    Put ("[Index_Backward] Ooops?");
  end if;
  if Index_Backward (s4_s4, +"cd", 8) /= 3 or
     Index_Backward (s4_s4,  "cd", 8) /= 3 or
     Index_Backward (s4_s4,  'c', 8)  /= 3
  then
    Put ("[Index_Backward, From] Ooops?");
  end if;
  --
  if Starts_With (+"package", 'q')     then Put ("Ooops?"); end if;
  if Starts_With (+"package",  "proc") then Put ("Ooops?"); end if;
  if Starts_With (+"package", +"proc") then Put ("Ooops?"); end if;
  if not Starts_With (+"package",  "pack") then Put ("Ooops?"); end if;
  if not Starts_With (+"package", +"pack") then Put ("Ooops?"); end if;
  --
  if Ends_With (+"package", 'f')     then Put ("Ooops?"); end if;
  if Ends_With (+"package",  "proc") then Put ("Ooops?"); end if;
  if Ends_With (+"package", +"proc") then Put ("Ooops?"); end if;
  if not Ends_With (+"package",  "age") then Put ("Ooops?"); end if;
  if not Ends_With (+"package", +"age") then Put ("Ooops?"); end if;
end Strings_demo;
