with HAC_Pack;  use HAC_Pack;

procedure Strings_demo is
  s1, s2, s4 : VString;
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
  end;
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
      Put_Line (Slice (v, 1, i) );
    end loop;
    --
    for i in 1 .. l loop
      Put ((i - 1) * ' ');
      Put_Line (Slice (v, i, l) );
    end loop;
    --
    for i in 1 .. l loop
      row := +"";
      for j in 1 .. l loop
        if i = j then
          c := Element (v, i);
        elsif abs (i-j) = 1 then
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
begin
  s2 := +"Hello";             --  Copy from literal
  s1 := s2;                   --  Copy VString to VString
  s4 := s1 & s3;              --  Concatenation VString & VString
  Put_Line (5 * (s1 & ' '));  --  Multiplication, and Concatenation with Character
  for i in 1 .. 4 loop Put ('.' & s1); end loop; New_Line;
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
  Put_Line (+"  [" & (-123) & "]  [" & 123 & "]  [" & 3.14159 & ']');
  Put_Line (123 & (+" Right VString"));
  Put_Line ((-456.0) & (+" Right VString"));
  Put_Line ((-456.0e9) & (+" Right VString"));
  Put_Line (456.0e11 & (+" Right VString"));
  Put_Line (456.0e66 & (+" Right VString"));
  Put_Line (456.789e13 & (+" Right VString"));
  Put_Line (4.56789e13 & (+" Right VString"));
  Put_Line (4.56789e14 & (+" Right VString"));
  Put_Line (4.56789e15 & (+" Right VString"));
  Put_Line (1.79769313486232E+307 & (+" Right VString"));
  --
  if s4 /= +"abcdef" then  --  Comparison VString to VString
    Put ("Ooops?");
  end if;
  if Length (s4) /= 6 then
    Put ("Ooops?");
  end if;
  if Index (s4, +"cd") /= 3 then
    Put ("Ooops?");
  end if;
end Strings_demo;
