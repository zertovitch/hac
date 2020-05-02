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
  s3 : constant VString := +" world";
  ZZ : Bi_Vector;
begin
  s2 := +"Hello";             --  Copy from literal
  s1 := s2;                   --  Copy VString to VString
  s4 := s1 & s3;              --  Concatenation
  for i in 1 .. 4 loop Put (s1 & ' '); end loop; New_Line;
  for i in 1 .. 4 loop Put ('.' & s1); end loop; New_Line;
  for i in 1 .. 4 loop Put ('"' & s1 & """    "); end loop; New_Line;
  s4 := "---> """ & s4 & '"';
  Put_Line (s4);
  Put_Line (">> " & s4 & ' ' & '!' & " <<");
  s4 := +"abc" & "def";
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
  --    if s4 = +"abcdef" then      --  Comparison VString to VString
  --      null;
  --    end if;
end Strings_demo;
