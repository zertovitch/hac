--  HAC_Pack - HAC Compatibility pack
-------------------------------------
--
--  This is the package containing all specifications of types and support
--  routines for HAC. So far all HAC programs must have "with" and "use"
--  of this package.
--  Note: this requirement is kept only for early stages of HAC.
--  At some point HAC_Pack won't be required anymore, but it is
--  still useful anyway for small script-like programs.
--
--  The package HAC_Pack is compilable by a real Ada compiler
--  like GNAT, so the HAC programs can be run on both HAC and
--  a "real" Ada system.
--
--  Another purpose of this specification is to document
--  the standard types and subprograms in HAC.

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Environment_Variables,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

pragma Warnings ("H");  --  Disable warning: declaration of "=" hides predefined operator.

package HAC_Pack is

  -----------------------------------------
  --  Floating-point numeric type: Real  --
  -----------------------------------------

  type Real is digits 15;
  package RIO is new Ada.Text_IO.Float_IO (Real);

  function   "**" (F1, F2 : Real)  return Real;

  -- Square Root
  function   Sqrt (I : Integer) return Real;
  function   Sqrt (F : Real   ) return Real;

  -- Integer to Character
  function   Chr       (I : Integer   ) return Character;

  -- Character to Integer
  function   Ord       (C : Character) return Integer;

  -- Next Character
  function   Succ      (C : Character) return Character;

  -- Previous Character
  function   Pred      (C : Character) return Character;

  -- Round to an Integer
  function   Round     (F : Real     ) return Integer;

  -- Truncate
  function   Trunc     (F : Real     ) return Integer;

  -- Trigonometric Functions     w/ arguments in radians
  function   Sin       (F : Real     ) return Real;
  function   Cos       (F : Real     ) return Real;
  function   Arctan    (F : Real     ) return Real;

  -- Exponential Functions
  function   Log       (F : Real     ) return Real;
  function   Exp       (F : Real     ) return Real;

  -- Random number in the real range [0, I+1[ , truncated to lowest integer.
  -- For example, Rand (10) returns equiprobable integer values
  -- between 0 and 10 (so, there are 11 possible values).
  function Rand (I : Integer) return Integer;

  -- Random number from 0 to 1, uniform.
  function Rnd return Real;

  package IIO is new Ada.Text_IO.Integer_IO (Integer);
  package BIO is new Ada.Text_IO.Enumeration_IO (Boolean);

  ------------------------------------------
  --  Variable-size string type: VString  --
  ------------------------------------------

  package VStr_Pkg renames Ada.Strings.Unbounded;
  subtype VString is VStr_Pkg.Unbounded_String;
  Null_VString : VString renames VStr_Pkg.Null_Unbounded_String;
  function To_VString (S : String) return VString renames VStr_Pkg.To_Unbounded_String;
  package ACH renames Ada.Characters.Handling;
  --
  function Element (Source : VString; Index : Positive) return Character renames VStr_Pkg.Element;
  function Ends_With (Item : VString; Pattern : String) return Boolean;
  function Ends_With (Item : VString; Pattern : VString) return Boolean;
  function Head (Source : VString; Count : Natural) return VString;
  function Index (Source : VString; Pattern : String) return Natural;
  function Index (Source : VString; Pattern : VString) return Natural;
  function Length (Source : VString) return Natural renames VStr_Pkg.Length;
  function Slice (Source : VString; From : Positive; To : Natural) return VString;
  function Tail (Source : VString; Count : Natural) return VString;
  function Starts_With (Item : VString; Pattern : String) return Boolean;
  function Starts_With (Item : VString; Pattern : VString) return Boolean;
  function To_Lower (Item : Character) return Character renames ACH.To_Lower;  --  RM A.3.2 (6)
  function To_Upper (Item : Character) return Character renames ACH.To_Upper;  --  RM A.3.2 (6)
  function To_Lower (Item : VString) return VString;
  function To_Upper (Item : VString) return VString;
  function Trim_Left  (Source : VString) return VString;
  function Trim_Right (Source : VString) return VString;
  function Trim_Both  (Source : VString) return VString;
  --
  function "+" (S : String) return VString renames To_VString;
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
  function Image_Attribute (F : Real) return VString;  --  returns +Real'Image(F) "as is"
  function Integer_Value (V: VString) return Integer;
  function Float_Value (V: VString) return Real;

  -------------------------
  --  Text Input/Output  --
  -------------------------
  --  1) Console I/O

  --  We have a real console/terminal input where several
  --  inputs can be made on the same line, followed by a
  --  "Return". It behaves like for a file. Actually it
  --  *could* be a file, if run like this: prog <input.txt .
  --
  function Get_Needs_Skip_Line return Boolean is (True);

  --  Get
  procedure Get (C : out Character) renames Ada.Text_IO.Get;
  procedure Get (I : out Integer  );
  procedure Get (F : out Real     );

  procedure Get_Immediate (C : out Character) renames Ada.Text_IO.Get_Immediate;

  --  Get and then move file pointer to next line (Skip_Line)
  procedure Get_Line (C : out Character);
  procedure Get_Line (I : out Integer  );
  procedure Get_Line (F : out Real     );
  procedure Get_Line (V : out VString  );  --  Gets the line till its end.

  procedure Skip_Line;

  --  Put
  procedure  Put (C     : Character);
  procedure  Put (I     : Integer;
                  Width : Ada.Text_IO.Field       := IIO.Default_Width;
                  Base  : Ada.Text_IO.Number_Base := IIO.Default_Base);
  procedure  Put (F     : Real;
                  Fore  : Integer := RIO.Default_Fore;
                  Aft   : Integer := RIO.Default_Aft;
                  Expo  : Integer := RIO.Default_Exp);
  procedure  Put (B     : Boolean;
                  Width : Ada.Text_IO.Field       := BIO.Default_Width );
  procedure  Put (S    :  String   );
  procedure  Put (V    :  VString  );

  --  Put and then New_Line (for S: it is the same as Ada.Text_IO.Put_Line)
  procedure  Put_Line (C     : Character);
  procedure  Put_Line (I     : Integer;
                       Width : Ada.Text_IO.Field       := IIO.Default_Width;
                       Base  : Ada.Text_IO.Number_Base := IIO.Default_Base);
  procedure  Put_Line (F    : Real;
                       Fore : Integer := RIO.Default_Fore;
                       Aft  : Integer := RIO.Default_Aft;
                       Expo : Integer := RIO.Default_Exp);
  procedure  Put_Line (B     : Boolean;
                       Width : Ada.Text_IO.Field := BIO.Default_Width);
  procedure  Put_Line (S    : String   );
  procedure  Put_Line (V    : VString  );

  procedure  New_Line;

  function End_Of_Line return Boolean renames Ada.Text_IO.End_Of_Line;
  function End_Of_File return Boolean renames Ada.Text_IO.End_Of_File;

  -------------------------
  --  Text Input/Output  --
  -------------------------
  --  2) File I/O

  subtype File_Type is Ada.Text_IO.File_Type;

  procedure Open (File : in out File_Type; Name : String);   --  Open as In_File (input).
  procedure Open (File : in out File_Type; Name : VString);  --  Open as In_File (input).

  procedure Create (File : in out File_Type; Name : String);   --  Create as Out_File (output).
  procedure Create (File : in out File_Type; Name : VString);  --  Create as Out_File (output).

  procedure Close (File : in out File_Type) renames Ada.Text_IO.Close;

  --  Get
  procedure Get (File : File_Type; C : out Character) renames Ada.Text_IO.Get;
  procedure Get (File : File_Type; I : out Integer  );
  procedure Get (File : File_Type; F : out Real     );

  --  Get and then move file pointer to next line (Skip_Line)
  procedure Get_Line (File : File_Type; C : out Character);
  procedure Get_Line (File : File_Type; I : out Integer  );
  procedure Get_Line (File : File_Type; F : out Real     );
  procedure Get_Line (File : File_Type; V : out VString  );  --  Gets the line till its end.

  procedure Skip_Line (File : File_Type);

  --  Put
  procedure  Put (File  : File_Type; C : Character);
  procedure  Put (File  : File_Type;
                  I     : Integer;
                  Width : Ada.Text_IO.Field       := IIO.Default_Width;
                  Base  : Ada.Text_IO.Number_Base := IIO.Default_Base);
  procedure  Put (File  : File_Type;
                  F     : Real;
                  Fore  : Integer := RIO.Default_Fore;
                  Aft   : Integer := RIO.Default_Aft;
                  Expo  : Integer := RIO.Default_Exp);
  procedure  Put (File  : File_Type;
                  B     : Boolean;
                  Width : Ada.Text_IO.Field       := BIO.Default_Width );
  procedure  Put (File : File_Type;
                  S    : String);
  procedure  Put (File : File_Type;
                  V    : VString);

  --  Put and then New_Line (for S: it is the same as Ada.Text_IO.Put_Line)
  procedure  Put_Line (File : File_Type;
                       C    : Character);
  procedure  Put_Line (File  : File_Type;
                       I     : Integer;
                       Width : Ada.Text_IO.Field       := IIO.Default_Width;
                       Base  : Ada.Text_IO.Number_Base := IIO.Default_Base);
  procedure  Put_Line (File : File_Type;
                       F    : Real;
                       Fore : Integer := RIO.Default_Fore;
                       Aft  : Integer := RIO.Default_Aft;
                       Expo : Integer := RIO.Default_Exp);
  procedure  Put_Line (File  : File_Type;
                       B     :  Boolean;
                       Width : Ada.Text_IO.Field := BIO.Default_Width);
  procedure  Put_Line (File : File_Type; S : String);
  procedure  Put_Line (File : File_Type; V : VString);

  procedure  New_Line (File : File_Type);

  function End_Of_Line (File : File_Type) return Boolean renames Ada.Text_IO.End_Of_Line;
  function End_Of_File (File : File_Type) return Boolean renames Ada.Text_IO.End_Of_File;

  --  Time

  subtype Time is Ada.Calendar.Time;
  function Clock return Time renames Ada.Calendar.Clock;

  --  Misc.

  type Semaphore is new Integer; -- private;

  --  Semaphore Procedures
  procedure  Wait      (S : Semaphore);
  procedure  Signal    (S : Semaphore);

  --  System

  function Argument_Count return Natural renames Ada.Command_Line.Argument_Count;
  function Argument (Number : Positive) return VString;

  function Get_Env (Name : String)  return VString;
  function Get_Env (Name : VString) return VString;

  procedure Set_Env (Name : String;  Value : String) renames Ada.Environment_Variables.Set;
  procedure Set_Env (Name : VString; Value : String);
  procedure Set_Env (Name : String;  Value : VString);
  procedure Set_Env (Name : VString; Value : VString);

  function Shell_Execute (Command : String) return Integer;
  function Shell_Execute (Command : VString) return Integer;

  function Directory_Separator return Character;

  --  This is public, but directly used by the HAC system itself only
  --  (HAC programs cannot return String's).
  --  That way, we avoid code duplication or incompatibilities between
  --  HAC_Pack (as compatibility package) and the HAC run-tim system itself.

  generic
    type Abstract_Integer is range <>;
  function HAC_Generic_Image (I : Abstract_Integer) return String;

  function HAC_Image (F : Real) return String;

  function HAC_Image (T : Ada.Calendar.Time) return String;

private

  -- type       SEMAPHORE is new INTEGER;

end HAC_Pack;
