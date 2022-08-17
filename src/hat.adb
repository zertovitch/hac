with Ada.Containers.Hashed_Maps,
     Ada.Numerics.Float_Random,
     Ada.Numerics.Generic_Elementary_Functions,
     Ada.Streams.Stream_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded.Hash,
     Ada.Text_IO.Text_Streams;

with Interfaces;

package body HAT is

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
    res : Integer;
  begin
    loop
      res := Trunc (Rnd * Real (I + 1));
      exit when res < I + 1;  --  In extremely rare cases we have res = I + 1.
    end loop;
    return res;
  end Rand;

  gen : Ada.Numerics.Float_Random.Generator;

  function Rnd return Real is
  begin
    return Real (Ada.Numerics.Float_Random.Random (gen));
  end Rnd;

  function HAC_Image (I : Integer) return String is
    Im : constant String := Integer'Image (I);
  begin
    --  Return image without leading ' ' on non-negative values.
    return (if I < 0 then Im else Im (Im'First + 1 .. Im'Last));
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
        return (if Seconds then ':' & ssc (ssc'Last - 1 .. ssc'Last) else "");
      end Optional_seconds;
      --
      --  The "if expression" version of that function
      --  confuses ObjectAda 10.4.
      --
      --  function Optional_intra_day return String is
      --  begin
      --    return
      --     (if Intra_day then
      --        "  " & shr (shr'Last - 1 .. shr'Last) & ':' &
      --        smn (smn'Last - 1 .. smn'Last) & Optional_seconds
      --      else
      --        "");
      --  end Optional_intra_day;

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

  function Slice (Source : VString; Low : Positive; High : Natural) return VString
  is
  begin
    return +VStr_Pkg.Slice (Source, Low, High);
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

  function "&" (D : Duration; V : VString) return VString is
  begin
    return Image (D) & V;
  end "&";

  function "&" (V : VString; D : Duration) return VString is
  begin
    return V & Image (D);
  end "&";

  function Nice_Image (B : Boolean) return VString is
    img : constant String := Boolean'Image (B);
  begin
    return +(img (img'First) & ACH.To_Lower (img (img'First + 1 .. img'Last)));
  end Nice_Image;

  function "&" (B : Boolean; V : VString) return VString is
  begin
    return Nice_Image (B) & V;
  end "&";

  function "&" (V : VString; B : Boolean) return VString is
  begin
    return V & Nice_Image (B);
  end "&";

  function To_Lower (Item : VString) return VString is
  begin
    return +Ada.Characters.Handling.To_Lower (VStr_Pkg.To_String (Item));
  end To_Lower;

  function To_Upper (Item : VString) return VString is
  begin
    return +Ada.Characters.Handling.To_Upper (VStr_Pkg.To_String (Item));
  end To_Upper;

  function Head (Source : VString; Count : Natural) return VString is
  begin
    return VStr_Pkg.Head (Source, Count);  --  We use the default padding: ' '.
  end Head;

  function Tail (Source : VString; Count : Natural) return VString is
  begin
    return VStr_Pkg.Tail (Source, Count);  --  We use the default padding: ' '.
  end Tail;

  function Head_Before_Match (Source : VString; Pattern : Character) return VString is
  begin
    return Head_Before_Match (Source, (1 => Pattern));
  end Head_Before_Match;

  function Head_Before_Match (Source : VString; Pattern : String) return VString is
  begin
    return Head_Before_Match (Source, +Pattern);
  end Head_Before_Match;

  function Head_Before_Match (Source : VString; Pattern : VString) return VString is
    i : constant Natural := Index (Source, Pattern);
  begin
    return (if i = 0 then Null_VString else Slice (Source, 1, i - 1));
  end Head_Before_Match;

  function Tail_After_Match (Source : VString; Pattern : Character) return VString is
  begin
    return Tail_After_Match (Source, (1 => Pattern));
  end Tail_After_Match;

  function Tail_After_Match (Source : VString; Pattern : String) return VString is
  begin
    return Tail_After_Match (Source, +Pattern);
  end Tail_After_Match;

  function Tail_After_Match (Source : VString; Pattern : VString) return VString is
    ------------------------------------------------------------------------------
    --  Description : Extract a substring from Source starting after the Pattern,
    --                to the end
    --  Arguments   : String to scan and Search pattern
    --  Return      : A substring from the character after the first Pattern
    --                found backward to the String's end
    --  Author      : Stéphane Rivière, 2021
    --
    --  Examples :
    --
    --  Tail_After_Match (+"/etc/genesix/gnx-startup",
    --    +"/")) returns "gnx-startup"
    --    +"ix")) returns "/gnx-startup"
    --    +"gene")) returns "six/gnx-startup"
    --    +"etc/genesix/gnx-startu")) returns "p"
    --    +"/etc/genesix/gnx-startu")) returns "p"
    --    +"/etc/genesix/gnx-startup")) returns empty string
    --    +"/etc/genesix/gnx-startupp")) returns empty string
    ------------------------------------------------------------------------------
    Result : VString := +"";
    Source_Length : constant Natural := Length (Source);
    Pattern_Length : constant Natural := Length (Pattern);
  begin
    for I in reverse 1 .. Source_Length - Pattern_Length loop
      if Slice (Source, I, I + Pattern_Length - 1) = Pattern then
        Result := Slice (Source, I + Pattern_Length, Source_Length);
        exit;
      end if;
    end loop;
    return Result;
  end Tail_After_Match;

  function Starts_With (Item : VString; Pattern : Character) return Boolean is
  begin
    return 1 <= Length (Item) and then Element (Item, 1) = Pattern;
  end Starts_With;

  function Starts_With (Item : VString; Pattern : String) return Boolean is
  begin
    return Pattern'Length <= Length (Item)
             and then VStr_Pkg.To_String (VStr_Pkg.Head (Item, Pattern'Length)) = Pattern;
  end Starts_With;

  function Starts_With (Item : VString; Pattern : VString) return Boolean is
  begin
    return Length (Pattern) <= Length (Item)
             and then VStr_Pkg.Head (Item, Length (Pattern)) = Pattern;
  end Starts_With;

  function Ends_With (Item : VString; Pattern : Character) return Boolean is
  begin
    return 1 <= Length (Item) and then Element (Item, Length (Item)) = Pattern;
  end Ends_With;

  function Ends_With (Item : VString; Pattern : String) return Boolean is
  begin
    return Pattern'Length <= Length (Item)
             and then VStr_Pkg.To_String (VStr_Pkg.Tail (Item, Pattern'Length)) = Pattern;
  end Ends_With;

  function Ends_With (Item : VString; Pattern : VString) return Boolean is
  begin
    return Length (Pattern) <= Length (Item)
             and then VStr_Pkg.Tail (Item, Length (Pattern)) = Pattern;
  end Ends_With;

  -------------
  --  Index  --
  -------------

  function Index (Source : VString; Pattern : Character) return Natural is
  begin
    return VStr_Pkg.Index (Source, (1 => Pattern));
  end Index;

  function Index (Source : VString; Pattern : String) return Natural is
  begin
    return VStr_Pkg.Index (Source, Pattern);
  end Index;

  function Index (Source : VString; Pattern : VString) return Natural is
  begin
    return VStr_Pkg.Index (Source, VStr_Pkg.To_String (Pattern));
  end Index;

  function Index (Source : VString; Pattern : Character; From : Positive) return Natural is
  begin
    return VStr_Pkg.Index (Source, (1 => Pattern), From);
  end Index;

  function Index (Source : VString; Pattern : String; From : Positive) return Natural is
  begin
    return VStr_Pkg.Index (Source, Pattern, From);
  end Index;

  function Index (Source : VString; Pattern : VString; From : Positive) return Natural is
  begin
    return VStr_Pkg.Index (Source, VStr_Pkg.To_String (Pattern), From);
  end Index;

  ----------------------
  --  Index_Backward  --
  ----------------------

  function Index_Backward (Source : VString; Pattern : Character) return Natural is
  begin
    return VStr_Pkg.Index (Source, (1 => Pattern), Ada.Strings.Backward);
  end Index_Backward;

  function Index_Backward (Source : VString; Pattern : String) return Natural is
  begin
    return VStr_Pkg.Index (Source, Pattern, Ada.Strings.Backward);
  end Index_Backward;

  function Index_Backward (Source : VString; Pattern : VString) return Natural is
  begin
    return VStr_Pkg.Index (Source, VStr_Pkg.To_String (Pattern), Ada.Strings.Backward);
  end Index_Backward;

  function Index_Backward (Source : VString; Pattern : Character; From : Positive) return Natural is
  begin
    return VStr_Pkg.Index (Source, (1 => Pattern), From, Ada.Strings.Backward);
  end Index_Backward;

  function Index_Backward (Source : VString; Pattern : String; From : Positive) return Natural is
  begin
    return VStr_Pkg.Index (Source, Pattern, From, Ada.Strings.Backward);
  end Index_Backward;

  function Index_Backward (Source : VString; Pattern : VString; From : Positive) return Natural is
  begin
    return VStr_Pkg.Index (Source, VStr_Pkg.To_String (Pattern), From, Ada.Strings.Backward);
  end Index_Backward;

  function "*" (Num : Natural; Pattern : String) return VString is
  begin
    return +Ada.Strings.Fixed."*" (Num, Pattern);
  end "*";

  function Trim_Left  (Source : VString) return VString is
  begin
    return VStr_Pkg.Trim (Source, Ada.Strings.Left);
  end Trim_Left;

  function Trim_Right (Source : VString) return VString is
  begin
    return VStr_Pkg.Trim (Source, Ada.Strings.Right);
  end Trim_Right;

  function Trim_Both  (Source : VString) return VString is
  begin
    return VStr_Pkg.Trim (Source, Ada.Strings.Both);
  end Trim_Both;

  function Image (I : Integer) return VString is
    function HAC_Image_for_Integer is
      new HAT.HAC_Generic_Image (Abstract_Integer => Integer);
  begin
    return +HAC_Image_for_Integer (I);
  end Image;

  function Image (F : Real) return VString is
  begin
    return +HAC_Image (F);
  end Image;

  function Image (T : Ada.Calendar.Time) return VString is
  begin
    return +HAC_Image (T);
  end Image;

  function Image (D : Duration) return VString is
    Im : constant String := Duration'Image (D);
  begin
    --  Return image without leading ' ' on non-negative values.
    return (if D < 0.0 then +Im else +Im (Im'First + 1 .. Im'Last));
  end Image;

  function Integer_Value (V : VString) return Integer is
  begin
    return Integer'Value (VStr_Pkg.To_String (V));
  end Integer_Value;

  function Float_Value (V : VString) return Real is
  begin
    return Real'Value (VStr_Pkg.To_String (V));
  end Float_Value;

  procedure Open (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Open (File, In_File, Name);
  end Open;

  procedure Open (File : in out File_Type; Name : VString) is
  begin Open (File, VStr_Pkg.To_String (Name)); end Open;

  procedure Create (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Create (File, Out_File, Name);
  end Create;

  procedure Create (File : in out File_Type; Name : VString) is
  begin Create (File, VStr_Pkg.To_String (Name)); end Create;

  procedure Append (File : in out File_Type; Name : String) is
    use Ada.Text_IO;
  begin
    Open (File, Append_File, Name);
  end Append;

  procedure Append (File : in out File_Type; Name : VString) is
  begin
    Append (File, VStr_Pkg.To_String (Name));
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

  ---------
  -- PUT --
  ---------

  procedure Put (C     : Character) is
  begin
    Character'Write (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output), C);
  end Put;

  procedure Put (File  : File_Type; C : Character) is
  begin
    Character'Write (Ada.Text_IO.Text_Streams.Stream (File), C);
  end Put;

  procedure Put (B     : Boolean;
                 Width : Ada.Text_IO.Field := BIO.Default_Width)
  is
  begin
    BIO.Put (B, Width);
  end Put;

  procedure  Put (File  : File_Type;
                  B     : Boolean;
                  Width : Ada.Text_IO.Field       := BIO.Default_Width)
  is
  begin
    BIO.Put (File, B, Width);
  end Put;

  procedure Put (S : in String) is
  begin
    String'Write (Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Current_Output), S);
  end Put;

  procedure Put (File : File_Type; S : in String) is
  begin
    String'Write (Ada.Text_IO.Text_Streams.Stream (File), S);
  end Put;

  procedure Put (V : in VString) is begin Put (VStr_Pkg.To_String (V)); end Put;
  procedure Put (File : File_Type; V : in VString) is begin Put (File, VStr_Pkg.To_String (V)); end Put;

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

  procedure Put_Line (V : VString) is begin Put_Line (VStr_Pkg.To_String (V)); end Put_Line;
  procedure Put_Line (File : File_Type; V : VString) is begin Put_Line (File, VStr_Pkg.To_String (V)); end Put_Line;

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

  procedure Set_Exit_Status (Code : in Integer) is
  begin
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Code));
  end Set_Exit_Status;

  function Get_Env (Name : String) return VString is
    use Ada.Environment_Variables;
  begin
    return
      (if Ada.Environment_Variables.Exists (Name) then +Value (Name)
       else Null_VString);
  end Get_Env;

  function Get_Env (Name : VString) return VString is
  begin
    return Get_Env (VStr_Pkg.To_String (Name));
  end Get_Env;

  procedure Set_Env (Name : VString; Value : String) is
  begin
    Set_Env (VStr_Pkg.To_String (Name), Value);
  end Set_Env;

  procedure Set_Env (Name : String; Value : VString) is
  begin
    Set_Env (Name, VStr_Pkg.To_String (Value));
  end Set_Env;

  procedure Set_Env (Name : VString; Value : VString) is
  begin
    Set_Env (VStr_Pkg.To_String (Name), VStr_Pkg.To_String (Value));
  end Set_Env;

  package String_Maps is new Ada.Containers.Hashed_Maps
    (Key_Type        => VString,
     Element_Type    => VString,
     Hash            => Ada.Strings.Unbounded.Hash,
     Equivalent_Keys => HAT."=",
     "="             => HAT."=");

  --  We emulate here the pool of VM variables attached to the HAC VM.
  global_VM_variables : String_Maps.Map;

  function Get_VM_Variable (Name : String)  return VString is
  begin
    return Get_VM_Variable (+Name);
  end Get_VM_Variable;

  function Get_VM_Variable (Name : VString) return VString is
    cur : constant String_Maps.Cursor := global_VM_variables.Find (Name);
    use String_Maps;
  begin
    return
      (if cur = String_Maps.No_Element then Null_VString
       else String_Maps.Element (cur));
  end Get_VM_Variable;

  procedure Set_VM_Variable (Name : String;  Value : String) is
  begin
    Set_VM_Variable (+Name, +Value);
  end Set_VM_Variable;

  procedure Set_VM_Variable (Name : VString; Value : String) is
  begin
    Set_VM_Variable (Name, +Value);
  end Set_VM_Variable;

  procedure Set_VM_Variable (Name : String;  Value : VString) is
  begin
    Set_VM_Variable (+Name, Value);
  end Set_VM_Variable;

  procedure Set_VM_Variable (Name : VString; Value : VString) is
  begin
    global_VM_variables.Include (Name, Value);
  end Set_VM_Variable;

  function Current_Directory return VString is
  begin
    return +Ada.Directories.Current_Directory;
  end Current_Directory;

  procedure Set_Directory (Directory : VString) is
  begin
    Set_Directory (VStr_Pkg.To_String (Directory));
  end Set_Directory;

  procedure Copy_File (Source_Name : String; Target_Name : String) is
  begin
    Ada.Directories.Copy_File (Source_Name, Target_Name);  --  Form: default value "".
  end Copy_File;

  procedure Copy_File (Source_Name : VString; Target_Name : String) is
  begin
    Copy_File (VStr_Pkg.To_String (Source_Name), Target_Name);
  end Copy_File;

  procedure Copy_File (Source_Name : String; Target_Name : VString) is
  begin
    Copy_File (Source_Name, VStr_Pkg.To_String (Target_Name));
  end Copy_File;

  procedure Copy_File (Source_Name : VString; Target_Name : VString) is
  begin
    Copy_File (VStr_Pkg.To_String (Source_Name), VStr_Pkg.To_String (Target_Name));
  end Copy_File;

  procedure Delete_File (Name : VString) is
  begin
    Delete_File (VStr_Pkg.To_String (Name));
  end Delete_File;

  function Exists (Name : VString) return Boolean is
  begin
    return Exists (VStr_Pkg.To_String (Name));
  end Exists;

  function Directory_Exists (Name : String) return Boolean is
    use Ada.Directories;
  begin
    return Exists (Name) and then Kind (Name) = Directory;
  end Directory_Exists;

  function Directory_Exists (Name : VString) return Boolean is
  begin
    return Directory_Exists (VStr_Pkg.To_String (Name));
  end Directory_Exists;

  function File_Exists (Name : String) return Boolean is
    use Ada.Directories;
  begin
    return Exists (Name) and then Kind (Name) = Ordinary_File;
  end File_Exists;

  function File_Exists (Name : VString) return Boolean is
  begin
    return File_Exists (VStr_Pkg.To_String (Name));
  end File_Exists;

  procedure Rename (Old_Name : VString; New_Name : String) is
  begin
    Rename (VStr_Pkg.To_String (Old_Name), New_Name);
  end Rename;

  procedure Rename (Old_Name : String; New_Name : VString) is
  begin
    Rename (Old_Name, VStr_Pkg.To_String (New_Name));
  end Rename;

  procedure Rename (Old_Name : VString; New_Name : VString) is
  begin
    Rename (VStr_Pkg.To_String (Old_Name), VStr_Pkg.To_String (New_Name));
  end Rename;

  function HAC_Generic_Image (I : Abstract_Integer) return String is
    Im : constant String := Abstract_Integer'Image (I);
  begin
    --  Return image without leading ' ' on non-negative values.
    return (if I < 0 then Im else Im (Im'First + 1 .. Im'Last));
  end HAC_Generic_Image;

  largest_without_exponent : constant := 10.0 ** (Real'Digits - 1);

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
      for c of any loop
        exit when c = 'E';  --  Ignore exponent part.
        if c in '1' .. '9' then
          n := n + 1;
        end if;
      end loop;
      return n;
    end Count_Nonzero_Digits;
  begin
    if abs F >= largest_without_exponent then
      return Image_with_exponent;
    end if;
    --  Image without exponent (E).
    --  If the number is too large for this layout, the Layout_Error
    --  exception is raised and we call Image_with_exponent.
    RIO.Put (s, F, Exp => 0);
    declare
      F_image   : constant String := Real'Image (F);
      tolerance : constant := 1;
    begin
      if Count_Nonzero_Digits (s) + tolerance < Count_Nonzero_Digits (F_image) then
        return Image_with_exponent;
        --  Significant loss of significant digits.
        --  Typically 6.62607015e-34 is displayed as 0.0[0] unless the
        --  type Real has a 34 digits precision (unlikely...). See Strings test.
        --  The tolerance is due to the case where s is, e.g., "123.4567890"
        --  and F_image is "1.234567891E+02". Spot the last significant
        --  digit: '0' vs. '1'. Such an accuracy error is normal.
      end if;
    end;
    --  We don't want to lose any digit in the decimal representation without exponent.
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

  package Non_Standard is

    procedure Sys (Command : String; Result : out Integer);

    function Directory_Separator return Character;

  end Non_Standard;

  package body Non_Standard is separate;

  procedure Shell_Execute (Command : String; Result : out Integer) is
    --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
  begin
    Non_Standard.Sys (Command, Result);
  end Shell_Execute;

  procedure Shell_Execute (Command : VString; Result : out Integer) is
  begin
    Shell_Execute (VStr_Pkg.To_String (Command), Result);
  end Shell_Execute;

  procedure Shell_Execute (Command : String) is
    Dummy : Integer;
  begin
    Shell_Execute (Command, Dummy);
  end Shell_Execute;

  procedure Shell_Execute (Command : VString) is
  begin
    Shell_Execute (VStr_Pkg.To_String (Command));
  end Shell_Execute;

  --  Versions with outward piping:

  procedure Shell_Execute (Command : String; Result : out Integer; Output : out VString) is
    package SIO renames Ada.Streams.Stream_IO;
    temp_1, temp_2 : SIO.File_Type;
  begin
    SIO.Create (temp_1, SIO.Out_File, "");
    declare
      temp_name_2 : constant String := SIO.Name (temp_1) & "_shell_exec.tmp";
    begin
      Non_Standard.Sys (Command & '>' & temp_name_2, Result);
      if Exists (temp_name_2) then
        SIO.Open (temp_2, SIO.In_File, temp_name_2);
        declare
          size : constant SIO.Count := SIO.Size (temp_2);
          buffer : String (1 .. Natural (size));
        begin
          if SIO.">" (size, 0) then
            String'Read (SIO.Stream (temp_2), buffer);
            Output := To_VString (buffer);
          else
            Output := Null_VString;
          end if;
        end;
        SIO.Delete (temp_2);
      else
        Output := Null_VString;
      end if;
    end;
    SIO.Close (temp_1);
  end Shell_Execute;

  procedure Shell_Execute (Command : VString; Result : out Integer; Output : out VString) is
  begin
    Shell_Execute (VStr_Pkg.To_String (Command), Result, Output);
  end Shell_Execute;

  procedure Shell_Execute (Command : String; Output : out VString) is
    Dummy : Integer;
  begin
    Shell_Execute (Command, Dummy, Output);
  end Shell_Execute;

  procedure Shell_Execute (Command : VString; Output : out VString) is
  begin
    Shell_Execute (VStr_Pkg.To_String (Command), Output);
  end Shell_Execute;

  function Directory_Separator return Character is
  begin
    return Non_Standard.Directory_Separator;
  end Directory_Separator;

begin
  pragma Assert
    (Real'Digits >= Interfaces.IEEE_Float_64'Digits,
     "HAT.Real must have at least the precision of IEEE Double Precision");
  Ada.Numerics.Float_Random.Reset (gen);  --  Randomize.
end HAT;
