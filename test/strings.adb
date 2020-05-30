with HAC_Pack;  use HAC_Pack;

procedure Strings is
  procedure Assert(Check : in Boolean) is  --  RM 11.4.2
  begin
    if not Check then Put_Line ("Assert fails"); end if;
  end;
  s1, s2, s3, s4 : VString;
  Planck   : constant := 6.62607015e-34;
  Pi       : constant := 3.141592653;
  Avogadro : constant := 6.02214076e023;
  r : Real;
  fs1 : String (4 .. 6);
begin
  s4 := +"abc" & 'd' & "ef";
  if s4 /= +"abcdef" then
    Put_Line ("Compiler bug [Comp. VString to VString, or conv. Literal String to VString]");
  end if;
  --
  if Element (s4, 3) /= 'c'    then Put_Line ("Compiler bug [VString Element]"); end if;
  if Length (s4) /= 6          then Put_Line ("Compiler bug [VString Length]"); end if;
  if Slice (s4, 3, 5) /= "cde" then Put_Line ("Compiler bug [VString Slice]"); end if;
  --
  s1 := +"ab";
  s2 := +"cdef";
  s3 := 'b' & s2;
  if s1 & s2 /= s4      then Put_Line ("Compiler bug [VString & VString]"); end if;
  if s1 & "cdef" /= s4  then Put_Line ("Compiler bug [VString & String]"); end if;
  if "ab" & s2 /= s4    then Put_Line ("Compiler bug [String  & VString]"); end if;
  if 'a' & s3 /= s4     then Put_Line ("Compiler bug [Character & VString]"); end if;
  if 7 & s1 /= +"7ab"   then Put_Line ("Compiler bug [Int & VString]"); end if;
  --
  if s1 & 7 /= +"ab7"   then Put_Line ("Compiler bug [VStr & Int = +Str_Lit]"); end if;
  if s1 & 7 /=  "ab7"   then Put_Line ("Compiler bug [VStr & Int =  Str_Lit]"); end if;
  --
  if 3.14 & s2 /= "3.14cdef"      then Put_Line ("Compiler bug [R & VString]"); end if;
  if s2 & Pi /= "cdef3.141592653" then Put_Line ("Compiler bug [VString & R]"); end if;
  if s2 & Avogadro /= +"cdef6.02214076E+23" then
    Put_Line (+"Compiler bug - HAC_Image for HAC_Float :" & Avogadro);
    Put_Line (Avogadro);
  end if;
  if s1 & Planck /= +"ab6.62607015E-34" then
    Put_Line (+"Compiler bug - HAC_Image for HAC_Float :" & Planck);
    Put_Line (Planck);
  end if;
  if not (+"A" < +"B")   then Put_Line ("Compiler bug [VString < VString]"); end if;
  if not (+"AA" > +"A")  then Put_Line ("Compiler bug [VString > VString]"); end if;
  --
  if not (+"A" <= +"B")  then Put_Line ("Compiler bug [VString <= VString]"); end if;
  if not (+"AA" >= +"A") then Put_Line ("Compiler bug [VString >= VString]"); end if;
  if not (+"A" <= +"A")  then Put_Line ("Compiler bug [VString <= VString]"); end if;
  if not (+"A" >= +"A")  then Put_Line ("Compiler bug [VString >= VString]"); end if;
  --
  if To_Lower (+"X") /= +"x" then Put_Line ("Compiler bug [To_Lower VString]"); end if;
  if To_Lower ( 'X') /=  'x' then Put_Line ("Compiler bug [To_Lower Char]");    end if;
  if To_Upper (+"x") /= +"X" then Put_Line ("Compiler bug [To_Upper VString]"); end if;
  if To_Upper ( 'x') /=  'X' then Put_Line ("Compiler bug [To_Upper Char]");    end if;
  --
  if Index (s4, +"cat") /= 0 then Put_Line ("Compiler bug [Index 1]");    end if;
  if Index (s4, +"cde") /= 3 then Put_Line ("Compiler bug [Index 2]");    end if;
  if Index (s4,  "cat") /= 0 then Put_Line ("Compiler bug [Index 3]");    end if;
  if Index (s4,  "cde") /= 3 then Put_Line ("Compiler bug [Index 4]");    end if;
  --
  if  0 * 'x' /= +""                       then Put_Line ("Compiler bug [* 1]"); end if;
  if 10 * 'x' /= +"xxxxxxxxxx"             then Put_Line ("Compiler bug [* 2]"); end if;
  if  0 * (+"Fritz") /= +""                then Put_Line ("Compiler bug [* 3]"); end if;
  if  3 * (+"Fritz") /= +"FritzFritzFritz" then Put_Line ("Compiler bug [* 4]"); end if;
  --
  for i in -5 .. 5 loop
    if Integer_Value (Image (i)) /= i then Put_Line ("Compiler bug [Im/Val I]"); end if;
    r := Real (i);
    if Float_Value (Image (r)) /= r then Put_Line ("Compiler bug [Im/Val R 1]"); end if;
    r := Real (i) * 1.0e20;
    if Float_Value (Image (r)) /= r then Put_Line ("Compiler bug [Im/Val R 2]"); end if;
    --  put_line (image(r));
  end loop;
  --
  fs1 := "def";
  if +fs1 /= Slice (s4, 4, 6) then
    Put_Line ("Compiler bug [Fixed String to VString]");
  end if;
  --
  if Starts_With (+"package",  "proc") then Put ("Ooops?"); end if;
  if Starts_With (+"package", +"proc") then Put ("Ooops?"); end if;
  if not Starts_With (+"package",  "pack") then Put ("Ooops?"); end if;
  if not Starts_With (+"package", +"pack") then Put ("Ooops?"); end if;
  --
  if Ends_With (+"package",  "proc") then Put ("Ooops?"); end if;
  if Ends_With (+"package", +"proc") then Put ("Ooops?"); end if;
  if not Ends_With (+"package",  "age") then Put ("Ooops?"); end if;
  if not Ends_With (+"package", +"age") then Put ("Ooops?"); end if;
  --  The following test is in one answer of
  --  https://stackoverflow.com/questions/62080743/how-do-you-check-if-string-ends-with-another-string-in-ada  
  Assert (Ends_With (+"John Johnson", "son") = True);
  Assert (Ends_With (+""  , ""  ) = True);
  Assert (Ends_With (+" " , ""  ) = True);
  Assert (Ends_With (+""  , " " ) = False);
  Assert (Ends_With (+" " , " " ) = True);
  Assert (Ends_With (+""  , "n" ) = False);
  Assert (Ends_With (+"n"  , "" ) = True);
  Assert (Ends_With (+"n ", "n ") = True);
  Assert (Ends_With (+" n", "n" ) = True);
  Assert (Ends_With (+"n" , " n") = False);
  Assert (Ends_With (+" n", " n") = True);
end Strings;
