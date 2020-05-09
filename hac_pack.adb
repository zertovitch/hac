with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Strings.Fixed; use Ada.Strings;

with Interfaces.C;

package body HAC_Pack is
  use Ada.Characters.Handling, VStrings_Pkg;

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
    return To_VString (VStrings_Pkg.Slice (Source, From, To));
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
    return To_VString (To_Lower (To_String (Item)));
  end;

  function To_Upper (Item : VString) return VString is
  begin
    return To_VString (To_Upper (To_String (Item)));
  end;

  function Index (Source : VString; Pattern : String) return Natural is
  begin
    return VStrings_Pkg.Index (Source, Pattern);
  end;

  function Index (Source : VString; Pattern : VString) return Natural is
  begin
    return VStrings_Pkg.Index (Source, To_String (Pattern));
  end;

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
    function HAC_Image is
      new HAC_Pack.HAC_Generic_Image (Abstract_Integer => Integer);
  begin
    return To_VString (HAC_Image (I));
  end Image;

  function Image (F : Real) return VString is
  begin
    return To_VString (HAC_Image (F));
  end;

  function Integer_Value (V: VString) return Integer is
  begin
    return Integer'Value (To_String (V));
  end;

  function Float_Value (V: VString) return Real is
  begin
    return Real'Value (To_String (V));
  end;

  package IIO is new Ada.Text_IO.Integer_IO(Integer);
  package RIO is new Ada.Text_IO.Float_IO(Real);
  package BIO is new Ada.Text_IO.Enumeration_IO(Boolean);

   ---------
   -- GET --
   ---------

  procedure Get (I : out Integer) is
  begin
    IIO.Get (I);
  end Get;

  procedure Get (F : out Real) is
  begin
    RIO.Get (F);
  end Get;

   --------------
   -- GET_LINE --
   --------------

  procedure Get_Line (C : out Character) is
  begin
    Get (C);
    Ada.Text_IO.Skip_Line;
  end Get_Line;

  procedure Get_Line (I : out Integer) is
  begin
    Get (I);
    Ada.Text_IO.Skip_Line;
  end Get_Line;

  procedure Get_Line (F : out Real) is
  begin
    Get (F);
    Ada.Text_IO.Skip_Line;
  end Get_Line;

  procedure Get_Line (V : out VString) is
  begin
    V := To_VString (Ada.Text_IO.Get_Line);
  end Get_Line;

  procedure Skip_Line is
  begin
    Ada.Text_IO.Skip_Line;  --  Without the optional parameter, Spacing.
  end;

  ---------
  -- PUT --
  ---------

  procedure Put (C : in  Character) is
  begin
    Ada.Text_IO.Put(C);
  end Put;

  procedure Put (I : in  Integer) is
  begin
    IIO.Put(I);
  end Put;

  procedure Put (I : in  Integer; W:  Width) is
  begin
    IIO.Put(I, W);
  end Put;

  procedure Put (F : in  Real) is
  begin
    RIO.Put(F);
  end Put;

  procedure Put (F : in  Real; W:  Width; D : Decimals) is
  begin
    RIO.Put(F,W,D);
  end Put;

  procedure Put (B : in  Boolean) is
  begin
    BIO.Put(B);
  end Put;

  procedure Put (S : in  String) is
  begin
    Ada.Text_IO.Put(S);
  end Put;

  procedure Put (V : in  VString) is
  begin
    Put (Ada.Strings.Unbounded.To_String (V));
  end Put;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (C : in  Character) is
  begin
    Put(C); New_Line;
  end Put_Line;

  procedure Put_Line (I : in  Integer) is
  begin
    Put(I); New_Line;
  end Put_Line;

  procedure Put_Line (I : in  Integer; W:  Width) is
  begin
    Put(I,W); New_Line;
  end Put_Line;

  procedure Put_Line (F : in  Real) is
  begin
    Put(F); New_Line;
  end Put_Line;

  procedure Put_Line (F : in  Real; W:  Width; D : Decimals) is
  begin
    Put(F,W,D); New_Line;
  end Put_Line;

  procedure Put_Line (B : in  Boolean) is
  begin
    Put(B); New_Line;
  end Put_Line;

  procedure Put_Line (S : in  String) is
  begin
    Ada.Text_IO.Put_Line(S);
  end Put_Line;

  procedure Put_Line (V : in  VString) is
  begin
    Put_Line (Ada.Strings.Unbounded.To_String (V));
  end Put_Line;

  procedure New_Line is
  begin
    Ada.Text_IO.New_Line;
  end New_Line;

  procedure  CursorAt (X, Y: Integer) is
  begin
    null; -- !!
  end;

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
    return To_VString (Ada.Command_Line.Argument (Number));
  end Argument;

  function Get_Env (Name : String) return VString is
    use Ada.Environment_Variables;
  begin
    if Exists (Name) then
      return To_VString (Value (Name));
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
      --  Banana skin: for a very small value, we'll have "0.0" from Put
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

  function Shell_Execute (Command : String) return Integer is
    use Interfaces.C;
    --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
    function Sys (Arg : char_array) return Integer;
    pragma Import(C, Sys, "system");
  begin
    return Sys (To_C (Command));
  end Shell_Execute;

  function Shell_Execute (Command : VString) return Integer is
  begin
    return Shell_Execute (To_String (Command));
  end;

begin
  Reset (gen);  --  Randomize.
end HAC_Pack;
