with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed; use Ada.Strings;

with Interfaces.C;

package body HAC_Pack is
  use Ada.Characters.Handling, VStr_Pkg;

  package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);

  function "**" (F1, F2 : Real) return Real is
  begin
    return REF."**" (F1, F2);
  end "**";

  function Sqrt (I : Integer) return Real is
  begin
    return REF.Sqrt (Real (I));
  end Sqrt;

  function Sqrt (F : Real) return Real is
  begin
    return REF.Sqrt (F);
  end Sqrt;

  function Chr (I : Integer) return Character is
  begin
    return Character'Val (I);
  end Chr;

  function Ord (C : Character) return Integer is
  begin
    return Character'Pos (C);
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
    return Integer (F);
  end Round;

  function Trunc (F : Real) return Integer is
  begin
    return Integer (Real'Floor (F));
  end Trunc;

  function Sin (F : Real) return Real is
  begin
    return REF.Sin (F);
  end Sin;

  function Cos (F : Real) return Real is
  begin
    return REF.Cos (F);
  end Cos;

  function Arctan (F : Real) return Real is
  begin
    return REF.Arctan (F);
  end Arctan;

  function Log (F : Real) return Real is
  begin
    return REF.Log (F);
  end Log;

  function Exp (F : Real) return Real is
  begin
    return REF.Exp (F);
  end Exp;

  function Rand (I : Integer) return Integer is
  begin
    return Trunc (Rnd * Real (I + 1));
  end Rand;

  gen : Generator;

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

  function HAC_Image (T : Ada.Calendar.Time) return String is
    use Ada;
    --  Time_display returns date & time, current or given.
    --  E.g.: "2013/08/01  05:49:51"
    --  Useful for a log file or a display of a lengthy operation.
    --  This is Ada 83 compatible. Format accepted by SQL queries.
    --
    --    32- or 64-bit: DEC/Compaq/HP Ada (83), GNAT (95/2005), ObjectAda (95)
    --    16-bit:        Meridian (83) -> Long_Integer is 32-bit
    --    16-bit:        Janus 2.x (83): KO: no Long_Integer
    --
    --  Test program in following comment:
    --
    --   with Text_IO,Time_display;procedure Test is begin Text_IO.Put(Time_display);end;
    --
    function Time_display (
      T        : Calendar.Time := Calendar.Clock;
      Seconds  : Boolean       := True;
      Intra_day : Boolean      := True
    )
      return String
    is
      subtype Sec_int is Long_Integer; -- must contain 86_400
      s : constant Sec_int := Sec_int (Calendar.Seconds (T));
      m : constant Sec_int := s / 60;
      --  + 100: trick for obtaining 0x
      sY : constant String := Integer'Image (Year (T));
      sM : constant String := Integer'Image (Month (T) + 100);
      sD : constant String := Integer'Image (Day (T)  + 100);
      shr : constant String := Sec_int'Image (m  /  60 + 100);
      smn : constant String := Sec_int'Image (m mod 60 + 100);
      ssc : constant String := Sec_int'Image (s mod 60 + 100);
      --
      function Optional_seconds return String is
      begin
        if Seconds then
          return ':' & ssc (ssc'Last - 1 .. ssc'Last);
        else
          return "";
        end if;
      end Optional_seconds;
      --
      function Optional_intra_day return String is
      begin
        if Intra_day then
          return
            "  " &
            shr (shr'Last - 1 .. shr'Last) & ':' &
            smn (smn'Last - 1 .. smn'Last) & Optional_seconds;
        else
          return "";
        end if;
      end Optional_intra_day;

    begin
      return
        sY (sY'Last - 3 .. sY'Last) & '/' &  -- not Year 10'000 compliant.
        sM (sM'Last - 1 .. sM'Last) & '/' &
        sD (sD'Last - 1 .. sD'Last) &
        Optional_intra_day;
    end Time_display;
  begin
    return Time_display (T);
  end HAC_Image;

  function To_VString (C : Character) return VString is
  begin
    return To_VString ((1 => C));
  end To_VString;

  function Slice (Source : VString; From : Positive; To : Natural) return VString
  is
  begin
    return +VStr_Pkg.Slice (Source, From, To);
  end Slice;

  function "&" (I : Integer; V : VString) return VString is
  begin
    return HAC_Image (I) & V;
  end "&";

  function "&" (V : VString; I : Integer) return VString is
  begin
    return V & HAC_Image (I);
  end "&";

  function "&" (R : Real; V : VString) return VString is
  begin
    return HAC_Image (R) & V;
  end "&";

  function "&" (V : VString; R : Real) return VString is
  begin
    return V & HAC_Image (R);
  end "&";

  function To_Lower (Item : VString) return VString is
  begin
    return +To_Lower (To_String (Item));
  end To_Lower;

  function To_Upper (Item : VString) return VString is
  begin
    return +To_Upper (To_String (Item));
  end To_Upper;

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
  end Trim_Left;

  function Trim_Right (Source : VString) return VString is
  begin
    return Trim (Source, Right);
  end Trim_Right;

  function Trim_Both  (Source : VString) return VString is
  begin
    return Trim (Source, Both);
  end Trim_Both;

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
    return +Real'Image (F);
  end Image_Attribute;

  function Image (T : Ada.Calendar.Time) return VString is
  begin
    return +HAC_Image (T);
  end Image;

  function Image (D : Duration) return VString is
  begin
    return +Duration'Image (D);
  end Image;

  function Integer_Value (V : VString) return Integer is
  begin
    return Integer'Value (To_String (V));
  end Integer_Value;

  function Float_Value (V : VString) return Real is
  begin
    return Real'Value (To_String (V));
  end Float_Value;

  procedure Open (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Open (File, In_File, Name);
  end Open;

  procedure Open (File : in out File_Type; Name : VString) is
  begin Open (File, To_String (Name)); end Open;

  procedure Create (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Create (File, Out_File, Name);
  end Create;

  procedure Create (File : in out File_Type; Name : VString) is
  begin Create (File, To_String (Name)); end Create;

  procedure Append (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Open (File, Append_File, Name);
  end Append;

  procedure Append (File : in out File_Type; Name : VString) is
  begin
    Append (File, To_String (Name));
  end Append;

   ---------
   -- GET --
   ---------

  procedure Get (I : out Integer) is begin IIO.Get (I); end Get;
  procedure Get (File : File_Type; I : out Integer) is begin IIO.Get (File, I); end Get;

  procedure Get (F : out Real) is begin RIO.Get (F); end Get;
  procedure Get (File : File_Type; F : out Real) is begin RIO.Get (File, F); end Get;

   --------------
   -- GET_LINE --
   --------------

  procedure Get_Line (C : out Character) is begin Get (C); Skip_Line; end Get_Line;
  procedure Get_Line (File : File_Type; C : out Character) is
    begin Get (File, C); Skip_Line (File); end Get_Line;

  procedure Get_Line (I : out Integer) is begin Get (I); Skip_Line; end Get_Line;
  procedure Get_Line (File : File_Type; I : out Integer) is
    begin Get (File, I); Skip_Line (File); end Get_Line;

  procedure Get_Line (F : out Real) is begin Get (F); Skip_Line; end Get_Line;
  procedure Get_Line (File : File_Type; F : out Real) is
    begin Get (File, F); Skip_Line (File); end Get_Line;

  procedure Get_Line (V : out VString) is begin V := +Ada.Text_IO.Get_Line; end Get_Line;
  procedure Get_Line (File : File_Type; V : out VString) is
    begin V := +Ada.Text_IO.Get_Line (File); end Get_Line;

  --  Ada.Text_IO's Skip_Line is called without the optional parameter, Spacing.
  procedure Skip_Line is begin Ada.Text_IO.Skip_Line; end Skip_Line;
  procedure Skip_Line (File : File_Type) is begin Ada.Text_IO.Skip_Line (File); end Skip_Line;

  ---------
  -- PUT --
  ---------

  procedure Put (C : in  Character) is begin Ada.Text_IO.Put (C); end Put;
  procedure Put (File : File_Type; C : in  Character) is begin Ada.Text_IO.Put (File, C); end Put;

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
                 Width : Ada.Text_IO.Field := BIO.Default_Width)
  is
  begin
    BIO.Put (B, Width);
  end Put;

  procedure  Put (File : File_Type;
                  B     : in  Boolean;
                  Width : Ada.Text_IO.Field       := BIO.Default_Width)
  is
  begin
    BIO.Put (File, B, Width);
  end Put;

  procedure Put (S : in String) is begin Ada.Text_IO.Put (S); end Put;
  procedure Put (File : File_Type; S : in String) is begin Ada.Text_IO.Put (File, S); end Put;

  procedure Put (V : in VString) is begin Put (To_String (V)); end Put;
  procedure Put (File : File_Type; V : in VString) is begin Put (File, To_String (V)); end Put;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (C : Character) is begin Put (C); New_Line; end Put_Line;
  procedure  Put_Line (File : File_Type; C : in Character) is
    begin Put (File, C); New_Line (File); end Put_Line;

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
                      Width : Ada.Text_IO.Field := BIO.Default_Width)
  is
  begin
    Put (B, Width);
    New_Line;
  end Put_Line;

  procedure Put_Line (File  : File_Type;
                      B     : Boolean;
                      Width : Ada.Text_IO.Field := BIO.Default_Width)
  is
  begin
    Put (File, B, Width);
    New_Line (File);
  end Put_Line;

  procedure Put_Line (S : String) is begin Ada.Text_IO.Put_Line (S); end Put_Line;
  procedure Put_Line (File : File_Type; S : String) is begin Ada.Text_IO.Put_Line (File, S); end Put_Line;

  procedure Put_Line (V : VString) is begin Put_Line (To_String (V)); end Put_Line;
  procedure Put_Line (File : File_Type; V : VString) is begin Put_Line (File, To_String (V)); end Put_Line;

  procedure New_Line is begin Ada.Text_IO.New_Line; end New_Line;
  procedure New_Line (File : File_Type) is begin Ada.Text_IO.New_Line (File); end New_Line;

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

  function Command_Name return VString is
  begin
    return +Ada.Command_Line.Command_Name;
  end Command_Name;

  function Get_Env (Name : String) return VString is
    use Ada.Environment_Variables;
  begin
    if Ada.Environment_Variables.Exists (Name) then
      return +Value (Name);
    else
      return Null_VString;
    end if;
  end Get_Env;

  function Get_Env (Name : VString) return VString is
  begin
    return Get_Env (To_String (Name));
  end Get_Env;

  procedure Set_Env (Name : VString; Value : String) is
  begin
    Set_Env (To_String (Name), Value);
  end Set_Env;

  procedure Set_Env (Name : String; Value : VString) is
  begin
    Set_Env (Name, To_String (Value));
  end Set_Env;

  procedure Set_Env (Name : VString; Value : VString) is
  begin
    Set_Env (To_String (Name), To_String (Value));
  end Set_Env;

  function Current_Directory return VString is
  begin
    return +Ada.Directories.Current_Directory;
  end Current_Directory;

  procedure Set_Directory (Directory : VString) is
  begin
    Set_Directory (To_String (Directory));
  end Set_Directory;

  procedure Copy_File (Source_Name : String; Target_Name : String) is
  begin
    Ada.Directories.Copy_File (Source_Name, Target_Name);  --  Form: default value "".
  end Copy_File;

  procedure Copy_File (Source_Name : VString; Target_Name : String) is
  begin
    Copy_File (To_String (Source_Name), Target_Name);
  end Copy_File;

  procedure Copy_File (Source_Name : String; Target_Name : VString) is
  begin
    Copy_File (Source_Name, To_String (Target_Name));
  end Copy_File;

  procedure Copy_File (Source_Name : VString; Target_Name : VString) is
  begin
    Copy_File (To_String (Source_Name), To_String (Target_Name));
  end Copy_File;

  procedure Delete_File (Name : VString) is
  begin
    Delete_File (To_String (Name));
  end Delete_File;

  function Exists (Name : VString) return Boolean is
  begin
    return Exists (To_String (Name));
  end Exists;

  procedure Rename (Old_Name : VString; New_Name : String) is
  begin
    Rename (To_String (Old_Name), New_Name);
  end Rename;

  procedure Rename (Old_Name : String; New_Name : VString) is
  begin
    Rename (Old_Name, To_String (New_Name));
  end Rename;

  procedure Rename (Old_Name : VString; New_Name : VString) is
  begin
    Rename (To_String (Old_Name), To_String (New_Name));
  end Rename;

  function HAC_Generic_Image (I : Abstract_Integer) return String is
    Im : constant String := Abstract_Integer'Image (I);
  begin
    if I < 0 then
      return Im;
    else
      return Im (Im'First + 1 .. Im'Last);  --  Remove the leading ' '.
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
    function Count_Nonzero_Digits (any : String) return Natural is
      n : Natural := 0;
    begin
      for i in any'Range loop
        exit when any (i) = 'E';  --  Ignore exponent part.
        if any (i) in '1' .. '9' then
          n := n + 1;
        end if;
      end loop;
      return n;
    end Count_Nonzero_Digits;
  begin
    --  Image without exponent (E).
    --  If the number is too large for this layout, a Layout_Error
    --  is raised and we call Image_with_exponent.
    RIO.Put (s, F, Exp => 0);
    if Count_Nonzero_Digits (s) < Count_Nonzero_Digits (Real'Image (F)) then
      return Image_with_exponent;  --  Loss of significant digits.
    end if;
    --  We don't lose any digits in decimal representation without exponent.
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
    pragma Import (C, Sys, "system");

    Directory_Separator : constant Character;
    pragma Import (C, Directory_Separator, "__gnat_dir_separator");
  end Non_Standard;

  procedure Shell_Execute (Command : String; Result : out Integer) is
    --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
  begin
    Result := Non_Standard.Sys (Interfaces.C.To_C (Command));
  end Shell_Execute;

  procedure Shell_Execute (Command : VString; Result : out Integer) is
  begin
    Shell_Execute (To_String (Command), Result);
  end Shell_Execute;

  procedure Shell_Execute (Command : String) is
    Dummy : Integer;
  begin
    Shell_Execute (Command, Dummy);
  end Shell_Execute;

  procedure Shell_Execute (Command : VString) is
  begin
    Shell_Execute (To_String (Command));
  end Shell_Execute;

  function Directory_Separator return Character is
  begin
    return Non_Standard.Directory_Separator;
  end Directory_Separator;

begin
  Reset (gen);  --  Randomize.
end HAC_Pack;
