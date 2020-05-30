with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed; use Ada.Strings;

with Interfaces.C;

package body HAC_Pack is
  use Ada.Characters.Handling, VStr_Pkg;

  package REF is new Ada.Numerics.Generic_Elementary_Functions(Real);

  function "**" (F1, F2 : Real) return Real is
  begin
    return REF."**" (F1, F2);
  end;

  function Sqrt (I : Integer) return Real is
  begin
    return REF.Sqrt(Real(I));
  end Sqrt;

  function Sqrt (F : Real) return Real is
  begin
    return REF.Sqrt(F);
  end Sqrt;

  function Chr (I : Integer) return Character is
  begin
    return Character'Val(I);
  end Chr;

  function Ord (C : Character) return Integer is
  begin
    return Character'Pos(C);
  end Ord;

  function Succ (C : Character) return Character is
  begin
    return Character'Succ (C);
  end Succ;

  function Pred (C : Character) return Character is
  begin
    return Character'Pred (C);
  end Pred;

  function Round (F : Real) return Integer is
  begin
    return Integer(F);
  end Round;

  function Trunc (F : Real) return Integer is
  begin
    return Integer(Real'Floor(F));
  end Trunc;

  function Sin (F : Real) return Real is
  begin
    return REF.Sin(F);
  end Sin;

  function Cos (F : Real) return Real is
  begin
    return REF.Cos(F);
  end Cos;

  function Arctan (F : Real) return Real is
  begin
    return REF.Arctan(F);
  end Arctan;

  function Log (F : Real) return Real is
  begin
    return REF.Log(F);
  end Log;

  function Exp (F : Real) return Real is
  begin
    return REF.Exp(F);
  end Exp;

  function Rand (I : Integer) return Integer is
  begin
    return Trunc (Rnd * Real(I + 1));
  end Rand;

  gen: Generator;

  function Rnd return Real is
  begin
    return Real (Random (gen));
  end Rnd;

  function HAC_Image (I : Integer) return String is
    Im : constant String := Integer'Image (I);
  begin
    if I < 0 then
      return Im;
    else
      return Im (Im'First + 1 .. Im'Last);
    end if;
  end HAC_Image;

  function Slice (Source : VString; From : Positive; To : Natural) return VString
  is
  begin
    return +VStr_Pkg.Slice (Source, From, To);
  end Slice;

  function "&" (I : Integer; V : VString) return VString is
  begin
    return HAC_Image (I) & V;
  end;

  function "&" (V : VString; I : Integer) return VString is
  begin
    return V & HAC_Image (I);
  end;

  function "&" (R : Real; V : VString) return VString is
  begin
    return HAC_Image (R) & V;
  end;

  function "&" (V : VString; R : Real) return VString is
  begin
    return V & HAC_Image (R);
  end;

  function To_Lower (Item : VString) return VString is
  begin
    return +To_Lower (To_String (Item));
  end;

  function To_Upper (Item : VString) return VString is
  begin
    return +To_Upper (To_String (Item));
  end;

  function Head (Source : VString; Count : Natural) return VString is
  begin
    return VStr_Pkg.Head (Source, Count);  --  We use the default padding: ' '.
  end Head;

  function Tail (Source : VString; Count : Natural) return VString is
  begin
    return VStr_Pkg.Tail (Source, Count);  --  We use the default padding: ' '.
  end Tail;

  function Starts_With (Item : VString; Pattern : String) return Boolean is
  begin
    return Pattern'Length <= Length (Item)
             and then To_String (VStr_Pkg.Head (Item, Pattern'Length)) = Pattern;
  end Starts_With;

  function Starts_With (Item : VString; Pattern : VString) return Boolean is
  begin
    return Length (Pattern) <= Length (Item)
             and then VStr_Pkg.Head (Item, Length (Pattern)) = Pattern;
  end Starts_With;

  function Ends_With (Item : VString; Pattern : String) return Boolean is
  begin
    return Pattern'Length <= Length (Item)
             and then To_String (VStr_Pkg.Tail (Item, Pattern'Length)) = Pattern;
  end Ends_With;

  function Ends_With (Item : VString; Pattern : VString) return Boolean is
  begin
    return Length (Pattern) <= Length (Item)
             and then VStr_Pkg.Tail (Item, Length (Pattern)) = Pattern;
  end Ends_With;

  function Index (Source : VString; Pattern : String) return Natural is
  begin
    return VStr_Pkg.Index (Source, Pattern);
  end Index;

  function Index (Source : VString; Pattern : VString) return Natural is
  begin
    return VStr_Pkg.Index (Source, To_String (Pattern));
  end Index;

  function "*" (Num : Natural; Pattern : String) return VString is
  begin
    return +Ada.Strings.Fixed."*" (Num, Pattern);
  end "*";

  function Trim_Left  (Source : VString) return VString is
  begin
    return Trim (Source, Left);
  end;

  function Trim_Right (Source : VString) return VString is
  begin
    return Trim (Source, Right);
  end;

  function Trim_Both  (Source : VString) return VString is
  begin
    return Trim (Source, Both);
  end;

  function Image (I : Integer) return VString is
    function HAC_Image_for_Integer is
      new HAC_Pack.HAC_Generic_Image (Abstract_Integer => Integer);
  begin
    return +HAC_Image_for_Integer (I);
  end Image;

  function Image (F : Real) return VString is
  begin
    return +HAC_Image (F);
  end Image;

  function Image_Attribute (F : Real) return VString is
  begin
    return +Real'Image(F);
  end Image_Attribute;

  function Integer_Value (V: VString) return Integer is
  begin
    return Integer'Value (To_String (V));
  end Integer_Value;

  function Float_Value (V: VString) return Real is
  begin
    return Real'Value (To_String (V));
  end Float_Value;

  procedure Open (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Open (File, In_File, Name);
  end Open;

  procedure Open (File : in out File_Type; Name : VString) is
    begin Open (File, To_String (Name)); end;

  procedure Create (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Create (File, Out_File, Name);
  end Create;

  procedure Create (File : in out File_Type; Name : VString) is
    begin Create (File, To_String (Name)); end;

   ---------
   -- GET --
   ---------

  procedure Get (I : out Integer) is begin IIO.Get (I); end;
  procedure Get (File : File_Type; I : out Integer) is begin IIO.Get (File, I); end;

  procedure Get (F : out Real) is begin RIO.Get (F); end;
  procedure Get (File : File_Type; F : out Real) is begin RIO.Get (File, F); end;

   --------------
   -- GET_LINE --
   --------------

  procedure Get_Line (C : out Character) is begin Get (C); Skip_Line; end;
  procedure Get_Line (File : File_Type; C : out Character) is
    begin Get (File, C); Skip_Line (File); end;

  procedure Get_Line (I : out Integer) is begin Get (I); Skip_Line; end;
  procedure Get_Line (File : File_Type; I : out Integer) is
    begin Get (File, I); Skip_Line (File); end;

  procedure Get_Line (F : out Real) is begin Get (F); Skip_Line; end;
  procedure Get_Line (File : File_Type; F : out Real) is
    begin Get (File, F); Skip_Line (File); end;

  procedure Get_Line (V : out VString) is begin V := +Ada.Text_IO.Get_Line; end;
  procedure Get_Line (File : File_Type; V : out VString) is
    begin V := +Ada.Text_IO.Get_Line (File); end;

  --  Ada.Text_IO's Skip_Line is called without the optional parameter, Spacing.
  procedure Skip_Line is begin Ada.Text_IO.Skip_Line; end;
  procedure Skip_Line (File : File_Type) is begin Ada.Text_IO.Skip_Line (File); end;

  ---------
  -- PUT --
  ---------

  procedure Put (C : in  Character) is begin Ada.Text_IO.Put (C); end;
  procedure Put (File : File_Type; C : in  Character) is begin Ada.Text_IO.Put (File, C); end;

  procedure  Put (I     : in  Integer;
                  Width : Ada.Text_IO.Field       := IIO.Default_Width;
                  Base  : Ada.Text_IO.Number_Base := IIO.Default_Base)
  is
  begin
    IIO.Put (I, Width, Base);
  end Put;

  procedure  Put (File  : File_Type;
                  I     : in  Integer;
                  Width : Ada.Text_IO.Field       := IIO.Default_Width;
                  Base  : Ada.Text_IO.Number_Base := IIO.Default_Base)
  is
  begin
    IIO.Put (File, I, Width, Base);
  end Put;

  procedure  Put (F    : in  Real;
                  Fore : Integer := RIO.Default_Fore;
                  Aft  : Integer := RIO.Default_Aft;
                  Expo : Integer := RIO.Default_Exp)
  is
  begin
    RIO.Put (F, Fore, Aft, Expo);
  end Put;

  procedure  Put (File : File_Type;
                  F     : in  Real;
                  Fore  : Integer := RIO.Default_Fore;
                  Aft   : Integer := RIO.Default_Aft;
                  Expo  : Integer := RIO.Default_Exp)
  is
  begin
    RIO.Put (File, F, Fore, Aft, Expo);
  end Put;

  procedure Put (B     : in  Boolean;
                 Width : Ada.Text_IO.Field := BIO.Default_Width )
  is
  begin
    BIO.Put (B, Width);
  end Put;

  procedure  Put (File : File_Type;
                  B     : in  Boolean;
                  Width : Ada.Text_IO.Field       := BIO.Default_Width )
  is
  begin
    BIO.Put (File, B, Width);
  end Put;

  procedure Put (S : in String) is begin Ada.Text_IO.Put (S); end;
  procedure Put (File : File_Type; S : in String) is begin Ada.Text_IO.Put (File, S); end;

  procedure Put (V : in VString) is begin Put (To_String (V)); end;
  procedure Put (File : File_Type; V : in VString) is begin Put (File, To_String (V)); end;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (C : Character) is begin Put(C); New_Line; end;
  procedure  Put_Line (File : File_Type; C : in Character) is
    begin Put(File, C); New_Line (File); end;

  procedure Put_Line (I     : Integer;
                      Width : Ada.Text_IO.Field       := IIO.Default_Width;
                      Base  : Ada.Text_IO.Number_Base := IIO.Default_Base)
  is
  begin
    Put (I, Width, Base);
    New_Line;
  end Put_Line;

  procedure  Put_Line (File  : File_Type;
                       I     : Integer;
                       Width : Ada.Text_IO.Field       := IIO.Default_Width;
                       Base  : Ada.Text_IO.Number_Base := IIO.Default_Base)
  is
  begin
    Put (File, I, Width, Base);
    New_Line (File);
  end Put_Line;

  procedure Put_Line (F    : Real;
                      Fore : Integer := RIO.Default_Fore;
                      Aft  : Integer := RIO.Default_Aft;
                      Expo : Integer := RIO.Default_Exp)
  is
  begin
    Put (F, Fore, Aft, Expo);
    New_Line;
  end Put_Line;

  procedure Put_Line (File : File_Type;
                      F    : Real;
                      Fore : Integer := RIO.Default_Fore;
                      Aft  : Integer := RIO.Default_Aft;
                      Expo : Integer := RIO.Default_Exp)
  is
  begin
    Put (File, F, Fore, Aft, Expo);
    New_Line (File);
  end Put_Line;

  procedure Put_Line (B     : Boolean;
                      Width : Ada.Text_IO.Field := BIO.Default_Width )
  is
  begin
    Put (B, Width);
    New_Line;
  end Put_Line;

  procedure Put_Line (File  : File_Type;
                      B     : Boolean;
                      Width : Ada.Text_IO.Field := BIO.Default_Width )
  is
  begin
    Put (File, B, Width);
    New_Line (File);
  end Put_Line;

  procedure Put_Line (S : String) is begin Ada.Text_IO.Put_Line (S); end;
  procedure Put_Line (File : File_Type; S : String) is begin Ada.Text_IO.Put_Line (File, S); end;

  procedure Put_Line (V : VString) is begin Put_Line (To_String (V)); end;
  procedure Put_Line (File : File_Type; V : VString) is begin Put_Line (File, To_String (V)); end;

  procedure New_Line is begin Ada.Text_IO.New_Line; end;
  procedure New_Line (File : File_Type) is begin Ada.Text_IO.New_Line (File); end;

  ----------
  -- WAIT --
  ----------

  procedure Wait (S : Semaphore) is
  begin
    raise Program_Error with "WAIT unimplemented";
  end Wait;

  ------------
  -- SIGNAL --
  ------------

  procedure Signal (S : Semaphore) is
  begin
    raise Program_Error with "SIGNAL unimplemented";
  end Signal;

  function Argument (Number : Positive) return VString is
  begin
    return +Ada.Command_Line.Argument (Number);
  end Argument;

  function Get_Env (Name : String) return VString is
    use Ada.Environment_Variables;
  begin
    if Exists (Name) then
      return +Value (Name);
    else
      return Null_VString;
    end if;
  end;

  function Get_Env (Name : VString) return VString is
  begin
    return Get_Env (To_String (Name));
  end;

  procedure Set_Env (Name : VString; Value : String) is
  begin
    Set_Env (To_String (Name), Value);
  end;

  procedure Set_Env (Name : String; Value : VString) is
  begin
    Set_Env (Name, To_String (Value));
  end;

  procedure Set_Env (Name : VString; Value : VString) is
  begin
    Set_Env (To_String (Name), To_String (Value));
  end;

  function HAC_Generic_Image (I : Abstract_Integer) return String is
    Im : constant String := Abstract_Integer'Image (I);
  begin
    if I < 0 then
      return Im;
    else
      return Im (Im'First + 1 .. Im'Last);
    end if;
  end HAC_Generic_Image;

  function HAC_Image (F : Real) return String is
    --  Code from TeXCAD (tc.adb, TeX_Number),
    --  less a few simplifications.
    s : String (1 .. Real'Digits + 15);
    na, nb, np, ne : Natural;
    function Image_with_exponent return String is
    begin
      RIO.Put (s, F);
      na := s'First;
      for i in s'Range loop
        case s (i) is
          when ' ' => na := i + 1;  --  * Trim spaces on the left
          when others => null;
        end case;
      end loop;
      ne := Ada.Strings.Fixed.Index (s, "0E");
      if ne > 0 then
        --  Simplify "4.56000000000000E+68" into "4.56E+68".
        --  * Remove extra '0's...
        nb := ne - 1;
        while s (nb) = '0' loop
          nb := nb - 1;
        end loop;
        if s (nb) = '.' then
          --  "4.E+68" from "4.00000000000000E+68" would be too much trimming...
          nb := nb + 1;  --  We keep one '0' -> "4.0E+68".
        end if;
        return s (na .. nb) & s (ne + 1 .. s'Last);
      end if;
      return s (na .. s'Last);
    end Image_with_exponent;
    --
    -- Ada 95 Quality and Style Guide, 7.2.7:
    -- Tests for
    --
    -- (1) absolute "equality" to 0 in storage,
    -- (2) absolute "equality" to 0 in computation,
    -- (3) relative "equality" to 0 in storage, and
    -- (4) relative "equality" to 0 in computation:
    --
    --  abs X <= Float_Type'Model_Small                      -- (1)
    --  abs X <= Float_Type'Base'Model_Small                 -- (2)
    --  abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
    --  abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)
    --
    function Almost_zero (x : Real) return Boolean is
    begin
      return abs x <= Real'Base'Model_Small;
    end Almost_zero;
    --
  begin
    if abs (F) < 10.0 ** (1 - Real'Digits)
      --  Banana skin 1: for a very small value, we'll have "0.0" from Put
      --  with Exp = 0 if we dont make this special case.
      --
      --  HAC sample code *with* the special case:
      --  ...
      --  for e in reverse -20 .. -1 loop
      --    Put_Line (+"" & 10.0 ** e);
      --  end loop;
      --  0.000000000001
      --  0.0000000000001
      --  0.00000000000001
      --  1.0E-15
      --  1.0E-16
      --  ...
      --
      --  !! TBD: Banana skin 2: loss of digits !!
      and then not Almost_zero (F)
      --  ^ Special case within the special case: for zero,
      --    we want to display 0.0 and not 0.0E+00
    then
      return Image_with_exponent;
    end if;
    RIO.Put (s, F, Exp => 0);  --  Image without exponent (E)
    na := s'First;
    nb := s'Last;
    np := 0;
    for i in s'Range loop
      case s (i) is
        when '.' => np := i; exit;    --  Find a decimal point
        when ' ' => na := i + 1;      --  * Trim spaces on the left
        when others => null;
      end case;
    end loop;
    if np > 0 then
      --  In case of a decimal point.
      while nb > np + 1 and then s (nb) = '0' loop
        nb := nb - 1;                 --  * Remove extra '0's except for "x.0"
      end loop;
    end if;
    return s (na .. nb);
  exception
    when Ada.Text_IO.Layout_Error =>
      --  Number too large, we fall back to show the version with exponent.
      return Image_with_exponent;
  end HAC_Image;

  --  Here is the non-Ada-standard stuff in HAC_Pack.
  package Non_Standard is
    function Sys (Arg : Interfaces.C.char_array) return Integer;
    pragma Import(C, Sys, "system");

    Directory_Separator : constant Character;
    pragma Import (C, Directory_Separator, "__gnat_dir_separator");
  end Non_Standard;

  function Shell_Execute (Command : String) return Integer is
    --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
  begin
    return Non_Standard.Sys (Interfaces.C.To_C (Command));
  end Shell_Execute;

  function Shell_Execute (Command : VString) return Integer is
  begin
    return Shell_Execute (To_String (Command));
  end Shell_Execute;

  function Directory_Separator return Character is
  begin
    return Non_Standard.Directory_Separator;
  end Directory_Separator;

begin
  Reset (gen);  --  Randomize.
end HAC_Pack;
