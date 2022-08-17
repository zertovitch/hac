--  HAT - HAC Ada Toolbox
-------------------------
--
--  The HAT package and possible children contains definitions
--  that are useful for HAC in its minimal operating mode.
--
--  HAT is compilable by a "full Ada" compiler like GNAT or ObjectAda,
--  so the HAC programs can be run on both HAC and a full Ada system.
--
--  Another purpose of this specification is to have a document,
--  automatically verified by "full Ada" systems, of the standard types
--  and subprograms available in HAC.
--
--  Furthermore, some items of HAT are used in the HAC virtual machine.
--  See occurrences of "HAT" in HAC.PCode.Interpreter's body.
------------------------
--
--  Legal licensing note:
--
--  Copyright (c) 2020 .. 2022 Gautier de Montmollin
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:
--
--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.
--
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.
--
--  NB: this is the MIT License, as found 12-Sep-2013 on the site
--  http://www.opensource.org/licenses/mit-license.php
--
-------------------------------------------------------------------------------------
--
with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Numerics,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with System;

--  Disable GNAT warning: declaration of "=" hides predefined operator.
pragma Warnings ("H");

package HAT is

  -----------------------------------------
  --  Floating-point numeric type: Real  --
  -----------------------------------------

  type Real is digits System.Max_Digits;
  package RIO is new Ada.Text_IO.Float_IO (Real);

  function   "**" (F1, F2 : Real)  return Real;

  --  Square Root
  function   Sqrt (I : Integer) return Real;
  function   Sqrt (F : Real)    return Real;

  --  Integer to Character
  function   Chr       (I : Integer)   return Character;

  --  Character to Integer
  function   Ord       (C : Character) return Integer;

  --  Next Character
  function   Succ      (C : Character) return Character;

  --  Previous Character
  function   Pred      (C : Character) return Character;

  --  Round to an Integer
  function   Round     (F : Real)      return Integer;

  --  Truncate
  function   Trunc     (F : Real)      return Integer;

  --  Min & Max
  function   Min       (I, J : Integer)   return Integer renames Integer'Min;
  function   Max       (I, J : Integer)   return Integer renames Integer'Max;
  function   Min       (F, G : Real)      return Real renames Real'Min;
  function   Max       (F, G : Real)      return Real renames Real'Max;

  Pi : constant := Ada.Numerics.Pi;

  --  Trigonometric Functions     w/ arguments in radians
  function   Sin       (F : Real)      return Real;
  function   Cos       (F : Real)      return Real;
  function   Arctan    (F : Real)      return Real;

  --  Exponential Functions
  function   Log       (F : Real)      return Real;
  function   Exp       (F : Real)      return Real;

  --  Random number in the real range [0, I+1[ , truncated to lowest integer.
  --  For example, Rand (10) returns equiprobable integer values
  --  from 0 to 10 (so, there are 11 possible values).
  function Rand (I : Integer) return Integer;

  --  Random number from 0 to 1, uniform.
  function Rnd return Real;

  package IIO is new Ada.Text_IO.Integer_IO (Integer);
  package BIO is new Ada.Text_IO.Enumeration_IO (Boolean);

  ------------------------------------------
  --  Variable-size string type: VString  --
  ------------------------------------------

  package VStr_Pkg renames Ada.Strings.Unbounded;  --  Could use XStrings instead.
  subtype VString is VStr_Pkg.Unbounded_String;
  Null_VString : VString renames VStr_Pkg.Null_Unbounded_String;
  function To_VString (S : String) return VString renames VStr_Pkg.To_Unbounded_String;
  function To_VString (C : Character) return VString;
  function To_String (V : VString) return String renames VStr_Pkg.To_String;
  package ACH renames Ada.Characters.Handling;
  --
  function Element (Source : VString; Index : Positive) return Character renames VStr_Pkg.Element;
  function Ends_With (Item : VString; Pattern : Character) return Boolean;
  function Ends_With (Item : VString; Pattern : String) return Boolean;
  function Ends_With (Item : VString; Pattern : VString) return Boolean;
  function Head (Source : VString; Count : Natural) return VString;
  --
  function Index (Source : VString; Pattern : Character) return Natural;
  function Index (Source : VString; Pattern : String) return Natural;
  function Index (Source : VString; Pattern : VString) return Natural;
  function Index (Source : VString; Pattern : Character; From : Positive) return Natural;
  function Index (Source : VString; Pattern : String; From : Positive) return Natural;
  function Index (Source : VString; Pattern : VString; From : Positive) return Natural;
  --
  function Index_Backward (Source : VString; Pattern : Character) return Natural;
  function Index_Backward (Source : VString; Pattern : String) return Natural;
  function Index_Backward (Source : VString; Pattern : VString) return Natural;
  function Index_Backward (Source : VString; Pattern : Character; From : Positive) return Natural;
  function Index_Backward (Source : VString; Pattern : String; From : Positive) return Natural;
  function Index_Backward (Source : VString; Pattern : VString; From : Positive) return Natural;
  --
  function Length (Source : VString) return Natural renames VStr_Pkg.Length;
  function Slice (Source : VString; Low : Positive; High : Natural) return VString;
  function Starts_With (Item : VString; Pattern : Character) return Boolean;
  function Starts_With (Item : VString; Pattern : String) return Boolean;
  function Starts_With (Item : VString; Pattern : VString) return Boolean;
  function Tail (Source : VString; Count : Natural) return VString;

  --  Head_Before_Match returns the head of Source preceding first occurence of Pattern.
  --  The result is empty if Pattern is not found.
  function Head_Before_Match (Source : VString; Pattern : Character) return VString;
  function Head_Before_Match (Source : VString; Pattern : String) return VString;
  function Head_Before_Match (Source : VString; Pattern : VString) return VString;

  --  Tail_After_Match returns the tail of Source following last occurence of Pattern.
  --  The result is empty if Pattern is not found.
  function Tail_After_Match (Source : VString; Pattern : Character) return VString;
  function Tail_After_Match (Source : VString; Pattern : String) return VString;
  function Tail_After_Match (Source : VString; Pattern : VString) return VString;

  function To_Lower (Item : Character) return Character renames ACH.To_Lower;  --  RM A.3.2 (6)
  function To_Upper (Item : Character) return Character renames ACH.To_Upper;  --  RM A.3.2 (6)
  function To_Lower (Item : VString) return VString;
  function To_Upper (Item : VString) return VString;

  function Trim_Left  (Source : VString) return VString;
  function Trim_Right (Source : VString) return VString;
  function Trim_Both  (Source : VString) return VString;
  --
  function "+" (S : String) return VString renames To_VString;
  function "+" (C : Character) return VString renames To_VString;
  function "-" (V : VString) return String renames To_String;
  --
  function "*" (Num : Natural; Pattern : Character) return VString renames VStr_Pkg."*";
  function "*" (Num : Natural; Pattern : String) return VString;
  function "*" (Num : Natural; Pattern : VString) return VString renames VStr_Pkg."*";
  --
  function "&" (V1, V2 : VString) return VString renames VStr_Pkg."&";
  --
  function "&" (V : VString; S : String) return VString renames VStr_Pkg."&";
  function "&" (S : String; V : VString) return VString renames VStr_Pkg."&";
  --
  function "&" (V : VString; C : Character) return VString renames VStr_Pkg."&";
  function "&" (C : Character; V : VString) return VString renames VStr_Pkg."&";
  --
  function "&" (I : Integer; V : VString) return VString;
  function "&" (V : VString; I : Integer) return VString;
  --
  function "&" (R : Real; V : VString) return VString;
  function "&" (V : VString; R : Real) return VString;
  --
  function "&" (D : Duration; V : VString) return VString;
  function "&" (V : VString; D : Duration) return VString;
  --
  function "&" (B : Boolean; V : VString) return VString;
  function "&" (V : VString; B : Boolean) return VString;
  --
  function "="  (Left, Right : VString) return Boolean renames VStr_Pkg."=";
  function "<"  (Left, Right : VString) return Boolean renames VStr_Pkg."<";
  function "<=" (Left, Right : VString) return Boolean renames VStr_Pkg."<=";
  function ">"  (Left, Right : VString) return Boolean renames VStr_Pkg.">";
  function ">=" (Left, Right : VString) return Boolean renames VStr_Pkg.">=";
  --
  function "="  (Left : VString;  Right : String) return Boolean renames VStr_Pkg."=";
  function "<"  (Left : VString;  Right : String) return Boolean renames VStr_Pkg."<";
  function "<=" (Left : VString;  Right : String) return Boolean renames VStr_Pkg."<=";
  function ">"  (Left : VString;  Right : String) return Boolean renames VStr_Pkg.">";
  function ">=" (Left : VString;  Right : String) return Boolean renames VStr_Pkg.">=";

  function Image (I : Integer) return VString;
  function Image (F : Real) return VString;            --  "nice" image of F
  function Image (T : Ada.Calendar.Time) return VString;
  function Image (D : Duration) return VString;

  function Integer_Value (V : VString) return Integer;
  function Float_Value (V : VString) return Real;

  -------------------------
  --  Text Input/Output  --
  --  1) Console I/O     --
  -------------------------

  --  We have a real console/terminal input where several
  --  inputs can be made on the same line, followed by a
  --  "Return". It behaves like for a file. Actually it
  --  *could* be a file, if run like this: prog <input.txt .
  --
  function Get_Needs_Skip_Line return Boolean is (True);

  --  Get
  procedure Get (C : out Character) renames Ada.Text_IO.Get;
  procedure Get (S : out String)    renames Ada.Text_IO.Get;
  procedure Get (I : out Integer);
  procedure Get (F : out Real);

  procedure Get_Immediate (C : out Character) renames Ada.Text_IO.Get_Immediate;

  --  Get and then move file pointer to next line (Skip_Line)
  procedure Get_Line (C : out Character);
  procedure Get_Line (I : out Integer);
  procedure Get_Line (F : out Real);
  procedure Get_Line (V : out VString);  --  Gets the line till its end.

  procedure Skip_Line (Spacing : Ada.Text_IO.Positive_Count := 1)
    renames Ada.Text_IO.Skip_Line;

  --  Put
  procedure Put (C     : Character);
  procedure Put (I     : Integer;
                 Width : Ada.Text_IO.Field       := IIO.Default_Width;
                 Base  : Ada.Text_IO.Number_Base := IIO.Default_Base) renames IIO.Put;
  procedure Put (F     : Real;
                 Fore  : Integer := RIO.Default_Fore;
                 Aft   : Integer := RIO.Default_Aft;
                 Expo  : Integer := RIO.Default_Exp) renames RIO.Put;
  procedure Put (B     : Boolean;
                 Width : Ada.Text_IO.Field       := BIO.Default_Width);
  procedure Put (S     : String);
  procedure Put (V     : VString);

  --  Put and then New_Line (for S: it is the same as Ada.Text_IO.Put_Line)
  procedure Put_Line (C     : Character);
  procedure Put_Line (I     : Integer;
                      Width : Ada.Text_IO.Field       := IIO.Default_Width;
                      Base  : Ada.Text_IO.Number_Base := IIO.Default_Base);
  procedure Put_Line (F     : Real;
                      Fore  : Integer := RIO.Default_Fore;
                      Aft   : Integer := RIO.Default_Aft;
                      Expo  : Integer := RIO.Default_Exp);
  procedure Put_Line (B     : Boolean;
                      Width : Ada.Text_IO.Field := BIO.Default_Width);
  procedure Put_Line (S     : String) renames Ada.Text_IO.Put_Line;
  procedure Put_Line (V     : VString);

  procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1)
    renames Ada.Text_IO.New_Line;

  function End_Of_Line return Boolean renames Ada.Text_IO.End_Of_Line;
  function End_Of_File return Boolean renames Ada.Text_IO.End_Of_File;

  -------------------------
  --  Text Input/Output  --
  --  2) File I/O        --
  -------------------------

  subtype File_Type is Ada.Text_IO.File_Type;

  --  Open file as input
  procedure Open (File : in out File_Type; Name : String);
  procedure Open (File : in out File_Type; Name : VString);

  --  Create file as output. If Name = "", it creates a temporary file.
  procedure Create (File : in out File_Type; Name : String);
  procedure Create (File : in out File_Type; Name : VString);

  --  Append to existing file (output).
  procedure Append (File : in out File_Type; Name : String);
  procedure Append (File : in out File_Type; Name : VString);

  procedure Close (File : in out File_Type) renames Ada.Text_IO.Close;

  --  Get
  procedure Get (File : File_Type; C : out Character) renames Ada.Text_IO.Get;
  procedure Get (File : File_Type; S : out String)    renames Ada.Text_IO.Get;
  procedure Get (File : File_Type; I : out Integer);
  procedure Get (File : File_Type; F : out Real);

  --  Get and then move file pointer to next line (Skip_Line)
  procedure Get_Line (File : File_Type; C : out Character);
  procedure Get_Line (File : File_Type; I : out Integer);
  procedure Get_Line (File : File_Type; F : out Real);
  procedure Get_Line (File : File_Type; V : out VString);     --  Gets the line till its end.

  procedure Skip_Line (File : File_Type; Spacing : Ada.Text_IO.Positive_Count := 1)
    renames Ada.Text_IO.Skip_Line;

  --  Put
  procedure Put (File  : File_Type; C : Character);
  procedure Put (File  : File_Type;
                 I     : Integer;
                 Width : Ada.Text_IO.Field       := IIO.Default_Width;
                 Base  : Ada.Text_IO.Number_Base := IIO.Default_Base) renames IIO.Put;
  procedure Put (File  : File_Type;
                 F     : Real;
                 Fore  : Integer := RIO.Default_Fore;
                 Aft   : Integer := RIO.Default_Aft;
                 Expo  : Integer := RIO.Default_Exp) renames RIO.Put;
  procedure Put (File  : File_Type;
                 B     : Boolean;
                 Width : Ada.Text_IO.Field       := BIO.Default_Width);
  procedure Put (File  : File_Type;
                 S     : String);
  procedure Put (File  : File_Type;
                 V     : VString);

  --  Put and then New_Line (for S: it is the same as Ada.Text_IO.Put_Line)
  procedure Put_Line (File  : File_Type;
                      C     : Character);
  procedure Put_Line (File  : File_Type;
                      I     : Integer;
                      Width : Ada.Text_IO.Field       := IIO.Default_Width;
                      Base  : Ada.Text_IO.Number_Base := IIO.Default_Base);
  procedure Put_Line (File  : File_Type;
                      F     : Real;
                      Fore  : Integer := RIO.Default_Fore;
                      Aft   : Integer := RIO.Default_Aft;
                      Expo  : Integer := RIO.Default_Exp);
  procedure Put_Line (File  : File_Type;
                      B     : Boolean;
                      Width : Ada.Text_IO.Field := BIO.Default_Width);
  procedure Put_Line (File  : File_Type;
                      S     : String) renames Ada.Text_IO.Put_Line;
  procedure Put_Line (File  : File_Type;
                      V     : VString);

  procedure New_Line (File : File_Type; Spacing : Ada.Text_IO.Positive_Count := 1)
    renames Ada.Text_IO.New_Line;

  function End_Of_Line (File : File_Type) return Boolean renames Ada.Text_IO.End_Of_Line;
  function End_Of_File (File : File_Type) return Boolean renames Ada.Text_IO.End_Of_File;
  function Is_Open     (File : File_Type) return Boolean renames Ada.Text_IO.Is_Open;

  ------------
  --  Time  --
  ------------

  subtype Time is Ada.Calendar.Time;

  function Clock return Time renames Ada.Calendar.Clock;
  function "-" (Left : Time; Right : Time) return Duration renames Ada.Calendar."-";
  --  The following functions are slightly different (no subtypes) from
  --  Ada.Calendar's but GNAT accepts the renaming. Is it correct?
  function Year    (Date : Time) return Integer renames Ada.Calendar.Year;
  function Month   (Date : Time) return Integer renames Ada.Calendar.Month;
  function Day     (Date : Time) return Integer renames Ada.Calendar.Day;
  function Seconds (Date : Time) return Duration renames Ada.Calendar.Seconds;

  -------------------------------------------------------------------------------------
  --  System (items similar to items in Ada.Directories, Ada.Environment_Variables)  --
  -------------------------------------------------------------------------------------

  function Argument_Count return Natural renames Ada.Command_Line.Argument_Count;
  function Argument (Number : Positive) return VString;
  function Command_Name return VString;
  procedure Set_Exit_Status (Code : in Integer);

  --  Get_Env: returns the value of environment variable Name.
  --  If Name is not set, returns an empty string.
  function Get_Env (Name : String)  return VString;
  function Get_Env (Name : VString) return VString;

  --  Set_Env: sets the value of environment variable Name.
  procedure Set_Env (Name : String;  Value : String) renames Ada.Environment_Variables.Set;
  procedure Set_Env (Name : VString; Value : String);
  procedure Set_Env (Name : String;  Value : VString);
  procedure Set_Env (Name : VString; Value : VString);

  --  Virtual Machine Variables work in a similar way as environment variables.
  --  The difference: they are visible only to an instance of the Virtual Machine,
  --  that is, to an object of type Builder.Build_Data.
  --  There are two ways of accessing them:
  --    1) from the HAC program via Get_VM_Variable, Set_VM_Variable below.
  --    2) from the "full Ada" application hosting HAC,
  --       via HAC_Sys.Interfacing.Get_VM_Variable, Set_VM_Variable.

  function Get_VM_Variable (Name : String)  return VString;
  function Get_VM_Variable (Name : VString) return VString;

  procedure Set_VM_Variable (Name : String;  Value : String);
  procedure Set_VM_Variable (Name : VString; Value : String);
  procedure Set_VM_Variable (Name : String;  Value : VString);
  procedure Set_VM_Variable (Name : VString; Value : VString);

  function Current_Directory return VString;

  procedure Set_Directory (Directory : String) renames Ada.Directories.Set_Directory;
  procedure Set_Directory (Directory : VString);

  procedure Copy_File (Source_Name : String;  Target_Name : String);
  procedure Copy_File (Source_Name : VString; Target_Name : String);
  procedure Copy_File (Source_Name : String;  Target_Name : VString);
  procedure Copy_File (Source_Name : VString; Target_Name : VString);

  procedure Delete_File (Name : String) renames Ada.Directories.Delete_File;
  procedure Delete_File (Name : VString);

  function Exists (Name : String) return Boolean renames Ada.Directories.Exists;
  function Exists (Name : VString) return Boolean;
  function Directory_Exists (Name : String) return Boolean;
  function Directory_Exists (Name : VString) return Boolean;
  function File_Exists (Name : String) return Boolean;
  function File_Exists (Name : VString) return Boolean;

  procedure Rename (Old_Name : String;  New_Name : String) renames Ada.Directories.Rename;
  procedure Rename (Old_Name : VString; New_Name : String);
  procedure Rename (Old_Name : String;  New_Name : VString);
  procedure Rename (Old_Name : VString; New_Name : VString);

  ---------------------
  --  Shell_Execute  --
  ---------------------

  procedure Shell_Execute (Command : String; Result : out Integer);
  procedure Shell_Execute (Command : VString; Result : out Integer);

  --  In this version, the result value is discarded:
  procedure Shell_Execute (Command : String);
  procedure Shell_Execute (Command : VString);

  --  The four above, but with piping of standard output into a VString:
  procedure Shell_Execute (Command : String; Result : out Integer; Output : out VString);
  procedure Shell_Execute (Command : VString; Result : out Integer; Output : out VString);
  procedure Shell_Execute (Command : String; Output : out VString);
  procedure Shell_Execute (Command : VString; Output : out VString);

  function Directory_Separator return Character;

  --  Semaphore stuff from SmallAda.
  --  It pre-dates Ada tasks and stems from CoPascal.
  type Semaphore is new Integer; -- private;
  procedure  Wait   (S : Semaphore);
  procedure  Signal (S : Semaphore);

  ---------------------------------
  --  End of compatibility part  --
  ---------------------------------
  --  The following part is formally public, but can only be directly
  --  used by the HAC system itself (HAC_Sys) or other "full Ada"
  --  programs using HAT (HAC programs cannot, so far, return String's
  --  or use generics).
  --  Those "private" items avoid code duplication or incompatibilities
  --  between HAT (as compatibility package) and the HAC run-time system
  --  itself.

  generic
    type Abstract_Integer is range <>;
  function HAC_Generic_Image (I : Abstract_Integer) return String;

  function HAC_Image (F : Real) return String;

  function HAC_Image (T : Ada.Calendar.Time) return String;

private

  --  type       SEMAPHORE is new INTEGER;

end HAT;
