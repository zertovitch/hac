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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Environment_Variables;
with Ada.Strings.Unbounded;

package HAC_Pack is

  -----------------------------------------
  --  Floating-point numeric type: Real  --
  -----------------------------------------

  type Real is digits 15;

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

  ------------------------------------------
  --  Variable-size string type: VString  --
  ------------------------------------------

  package VStrings_Pkg renames Ada.Strings.Unbounded;
  subtype VString is VStrings_Pkg.Unbounded_String;
  Null_VString : VString renames VStrings_Pkg.Null_Unbounded_String;
  function To_VString (S : String) return VString renames VStrings_Pkg.To_Unbounded_String;
  --
  function Element (Source : VString; Index : Positive) return Character
    renames VStrings_Pkg.Element;
  function Length (Source : VString) return Natural renames VStrings_Pkg.Length;
  function Slice (Source : VString; From : Positive; To : Natural) return VString;
  --
  function "+" (S : String) return VString renames To_VString;
  function "&" (V1, V2 : VString) return VString renames VStrings_Pkg."&";
  --
  function "&" (V : VString; S : String) return VString renames VStrings_Pkg."&";
  function "&" (S : String; V : VString) return VString renames VStrings_Pkg."&";
  --
  function "&" (V : VString; C : Character) return VString renames VStrings_Pkg."&";
  function "&" (C : Character; V : VString) return VString renames VStrings_Pkg."&";
  --
  function "&" (I : Integer; V : VString) return VString;
  function "&" (V : VString; I : Integer) return VString;
  --
  function "&" (R : Real; V : VString) return VString;
  function "&" (V : VString; R : Real) return VString;
  --
  function "="  (Left, Right : VString) return Boolean renames VStrings_Pkg."=";
  function "<"  (Left, Right : VString) return Boolean renames VStrings_Pkg."<";
  function "<=" (Left, Right : VString) return Boolean renames VStrings_Pkg."<=";
  function ">"  (Left, Right : VString) return Boolean renames VStrings_Pkg.">";
  function ">=" (Left, Right : VString) return Boolean renames VStrings_Pkg.">=";

  function To_Lower (Item : Character) return Character  --  RM A.3.2 (6)
    renames Ada.Characters.Handling.To_Lower;
  function To_Upper (Item : Character) return Character  --  RM A.3.2 (6)
    renames Ada.Characters.Handling.To_Upper;

  function To_Lower (Item : VString) return VString;
  function To_Upper (Item : VString) return VString;

  function Index (Source : VString; Pattern : VString) return Natural;
  function "*" (Left : Natural; Right : Character) return VString renames VStrings_Pkg."*";
  function "*" (Left : Natural; Right : VString) return VString renames VStrings_Pkg."*";
  function Trim_Left  (Source : VString) return VString;
  function Trim_Right (Source : VString) return VString;
  function Trim_Both  (Source : VString) return VString;

  -------------------------
  --  Text Input-Output  --
  -------------------------

  --  Get
  procedure Get (C : out Character);
  procedure Get (I : out Integer  );
  procedure Get (F : out Real     );
  procedure Get (B : out Boolean  );

  --  Get and then move file pointer to next line (Skip_Line)
  procedure Get_Line (C : out Character);
  procedure Get_Line (I : out Integer  );
  procedure Get_Line (F : out Real     );
  procedure Get_Line (B : out Boolean  );

  procedure Skip_Line;

  subtype Width is Positive;
  subtype Decimals is Positive;

  --  Put
  procedure  Put (C : in  Character);
  procedure  Put (I : in  Integer  );
  procedure  Put (I : in  Integer;  W:  Width);
  procedure  Put (F : in  Real    );
  procedure  Put (F : in  Real;    W:  Width; D : Decimals);
  procedure  Put (B : in  Boolean  );
  procedure  Put (S : in  String   );
  procedure  Put (V : in  VString  );

  --  Put and then New_Line ( !! it is the same as Ada.Text_IO only for S )
  procedure  Put_Line (C : in  Character);
  procedure  Put_Line (I : in  Integer  );
  procedure  Put_Line (I : in  Integer; W:  Width);
  procedure  Put_Line (F : in  Real    );
  procedure  Put_Line (F : in  Real;   W:  Width; D : Decimals);
  procedure  Put_Line (B : in  Boolean  );
  procedure  Put_Line (S : in  String   );
  procedure  Put_Line (V : in  VString  );

  --  Mark End of Line
  procedure  New_Line                      ;

  procedure  CursorAt (X, Y: Integer);

  type Semaphore is new Integer; -- private;

  --  Semaphore Procedures
  procedure  Wait      (S : Semaphore);
  procedure  Signal    (S : Semaphore);

  --  Misc.

  function Argument_Count return Natural renames Ada.Command_Line.Argument_Count;
  function Argument (Number : Positive) return VString;

  function Get_Env (Name : String)  return VString;
  function Get_Env (Name : VString) return VString;

  procedure Set_Env (Name : String;  Value : String) renames Ada.Environment_Variables.Set;
  procedure Set_Env (Name : VString; Value : String);
  procedure Set_Env (Name : String;  Value : VString);
  procedure Set_Env (Name : VString; Value : VString);

  --  This is public, but principally used by HAC itself to avoid too much code duplication.

  function HAC_Image (F : Real) return String;

private

  -- type       SEMAPHORE is new INTEGER;

end HAC_Pack;
