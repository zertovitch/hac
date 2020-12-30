with HAC_Pack;  use HAC_Pack;

procedure Strings is
  procedure Failure (Msg: VString) is
  begin
    Put_Line (+"Failure in test: " & Msg);    
    Set_Exit_Status (1);  --  Compiler test failed.
  end;
  --
  procedure Assert(Check : in Boolean) is  --  Similar to RM 11.4.2
  begin
    if not Check then Failure (+"Assert fails"); end if;
  end;
  s1, s2, s3, s4, s4_s4 : VString;
  Planck   : constant := 6.62607015e-34;
  Pi       : constant := 3.141592653;
  Avogadro : constant := 6.02214076e023;
  r : Real;
  fs1 : String (4 .. 6);
begin
  s4 := +"abc" & 'd' & "ef";
  if s4 /= +"abcdef" then
    Failure (+"Compiler bug [Comp. VString to VString, or conv. Literal String to VString]");
  end if;
  --
  if Element (s4, 3) /= 'c'    then Failure (+"Compiler bug [VString Element]"); end if;
  if Length (s4) /= 6          then Failure (+"Compiler bug [VString Length]"); end if;
  if Slice (s4, 3, 5) /= "cde" then Failure (+"Compiler bug [VString Slice]"); end if;
  --
  s1 := +"ab";
  s2 := +"cdef";
  s3 := 'b' & s2;
  if s1 & s2 /= s4      then Failure (+"Compiler bug [VString & VString]"); end if;
  if s1 & "cdef" /= s4  then Failure (+"Compiler bug [VString & String]"); end if;
  if "ab" & s2 /= s4    then Failure (+"Compiler bug [String  & VString]"); end if;
  if 'a' & s3 /= s4     then Failure (+"Compiler bug [Character & VString]"); end if;
  if 7 & s1 /= +"7ab"   then Failure (+"Compiler bug [Int & VString]"); end if;
  --
  if s1 & 7 /= +"ab7"   then Failure (+"Compiler bug [VStr & Int = +Str_Lit]"); end if;
  if s1 & 7 /=  "ab7"   then Failure (+"Compiler bug [VStr & Int =  Str_Lit]"); end if;
  --
  if 3.14 & s2 /= "3.14cdef"      then Failure (+"Compiler bug [R & VString]"); end if;
  if s2 & Pi /= "cdef3.141592653" then Failure (+"Compiler bug [VString & R]"); end if;
  if s2 & Avogadro /= +"cdef6.02214076E+23" then
    Failure (+"Compiler bug - HAC_Image for HAC_Float :" & Avogadro);
    Put_Line (Avogadro);
  end if;
  if s1 & Planck /= +"ab6.62607015E-34" then
    Failure (+"Compiler bug - HAC_Image for HAC_Float :" & Planck);
    Put_Line (Planck);
  end if;
  if not (+"A" < +"B")   then Failure (+"VString < VString"); end if;
  if not (+"AA" > +"A")  then Failure (+"VString > VString"); end if;
  --
  if not (+"A" <= +"B")  then Failure (+"VString <= VString"); end if;
  if not (+"AA" >= +"A") then Failure (+"VString >= VString"); end if;
  if not (+"A" <= +"A")  then Failure (+"VString <= VString"); end if;
  if not (+"A" >= +"A")  then Failure (+"VString >= VString"); end if;
  --
  if To_Lower (+"X") /= +"x" then Failure (+"To_Lower VString"); end if;
  if To_Lower ( 'X') /=  'x' then Failure (+"To_Lower Char");    end if;
  if To_Upper (+"x") /= +"X" then Failure (+"To_Upper VString"); end if;
  if To_Upper ( 'x') /=  'X' then Failure (+"To_Upper Char");    end if;
  --
  if Index (s4, +"cat") /= 0 then Failure (+"Index 1");    end if;
  if Index (s4, +"cde") /= 3 then Failure (+"Index 2");    end if;
  if Index (s4,  "cat") /= 0 then Failure (+"Index 3");    end if;
  if Index (s4,  "cde") /= 3 then Failure (+"Index 4");    end if;
  --
  s4_s4 := s4 & s4;  --  abcdefabcdef
  if Index_Backward (s4_s4, +"cd") /= 9 or
     Index_Backward (s4_s4,  "cd") /= 9 or
     Index_Backward (s4_s4,  'c')  /= 9
  then
    Failure (+"Index_Backward");
  end if;
  --
  if  0 * 'x' /= +""                       then Failure (+"""*"", #1"); end if;
  if 10 * 'x' /= +"xxxxxxxxxx"             then Failure (+"""*"", #2"); end if;
  if  0 * (+"Fritz") /= +""                then Failure (+"""*"", #3"); end if;
  if  3 * (+"Fritz") /= +"FritzFritzFritz" then Failure (+"""*"", #4"); end if;
  --
  for i in -5 .. 5 loop
    if Integer_Value (Image (i)) /= i then Failure (+"Im/Val I"); end if;
    r := Real (i);
    if Float_Value (Image (r)) /= r then Failure (+"Im/Val R 1"); end if;
    r := Real (i) * 1.0e20;
    if Float_Value (Image (r)) /= r then Failure (+"Im/Val R 2"); end if;
    --  put_line (image(r));
  end loop;
  --
  fs1 := "def";
  if +fs1 /= Slice (s4, 4, 6) then
    Failure (+"Fixed String to VString");
  end if;
  --
  if Starts_With (+"package",  "proc") then Failure (+"Starts_With"); end if;
  if Starts_With (+"package", +"proc") then Failure (+"Starts_With"); end if;
  if not Starts_With (+"package",  "pack") then Failure (+"Starts_With"); end if;
  if not Starts_With (+"package", +"pack") then Failure (+"Starts_With"); end if;
  --
  if Ends_With (+"package",  "proc") then Failure (+"Ends_With"); end if;
  if Ends_With (+"package", +"proc") then Failure (+"Ends_With"); end if;
  if not Ends_With (+"package",  "age") then Failure (+"Ends_With"); end if;
  if not Ends_With (+"package", +"age") then Failure (+"Ends_With"); end if;
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
