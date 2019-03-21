with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body HAC_Pack is

  package FEF is new Ada.Numerics.Generic_Elementary_Functions(Float);

  function Sqr (I : Integer) return Integer is
  begin
    return I*I;
  end Sqr;

  function Sqr (F : Float) return Float is
  begin
    return F*F;
  end Sqr;

  function   "**"      (f1, f2 : Float)  return Float is
  begin
    return FEF."**" (f1, f2);
  end;

  function Sqrt (I : Integer) return Float is
  begin
    return FEF.Sqrt(Float(I));
  end Sqrt;

  function Sqrt (F : Float) return Float is
  begin
    return FEF.Sqrt(F);
  end Sqrt;

  function Odd (I : Integer) return Boolean is
  begin
    return I mod 2 = 1;
  end Odd;

  function ASCII (I : Integer) return Character is
  begin
    return Character'Val(I);
  end ASCII;

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

  function Round (F : Float) return Integer is
  begin
    return Integer(F);
  end Round;

  function Trunc (F : Float) return Integer is
  begin
    return Integer(Float'Floor(F));
  end Trunc;

  function Sin (I : Integer) return Float is
  begin
    return FEF.Sin(Float(I));
  end Sin;

  function Sin (F : Float) return Float is
  begin
    return FEF.Sin(F);
  end Sin;

  function Cos (I : Integer) return Float is
  begin
    return FEF.Cos(Float(I));
  end Cos;

  function Cos (F : Float) return Float is
  begin
    return FEF.Cos(F);
  end Cos;

  function ArcTan (I : Integer) return Float is
  begin
    return FEF.Arctan(Float(I));
  end ArcTan;

  function ArcTan (F : Float) return Float is
  begin
    return FEF.Arctan(F);
  end ArcTan;

  function Log (I : Integer) return Float is
  begin
    return FEF.Log(Float(I));
  end Log;

  function Log (F : Float) return Float is
  begin
    return FEF.Log(F);
  end Log;

  function Exp (I : Integer) return Float is
  begin
    return FEF.Exp(Float(I));
  end Exp;

  function Exp (F : Float) return Float is
  begin
    return FEF.Exp(F);
  end Exp;

  gen: Generator;

  function Random (I : Integer) return Integer is
  begin
    return Trunc(Random(I));
  end Random;

  function Random (I : Integer) return Float is
  begin
    return Random(gen) * Float(I);
  end Random;

  package IIO is new Ada.Text_IO.Integer_IO(Integer);
  package FIO is new Ada.Text_IO.Float_IO(Float);
  package BIO is new Ada.Text_IO.Enumeration_IO(Boolean);

   ---------
   -- GET --
   ---------

  procedure Get (C : out Character) is
  begin
    Ada.Text_IO.Get(C);
  end Get;

   ---------
   -- GET --
   ---------

  procedure Get (I : out Integer) is
  pragma Unreferenced (I);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET unimplemented");
    raise Program_Error;
  end Get;

   ---------
   -- GET --
   ---------

  procedure Get (F : out Float) is
  pragma Unreferenced (F);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET unimplemented");
    raise Program_Error;
  end Get;

   ---------
   -- GET --
   ---------

  procedure Get (B : out Boolean) is
  pragma Unreferenced (B);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET unimplemented");
    raise Program_Error;
  end Get;

   --------------
   -- GET_LINE --
   --------------

  procedure Get_Line (C : out Character) is
  pragma Unreferenced (C);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end Get_Line;

   --------------
   -- GET_LINE --
   --------------

  procedure Get_Line (I : out Integer) is
  pragma Unreferenced (I);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end Get_Line;

   --------------
   -- GET_LINE --
   --------------

  procedure Get_Line (F : out Float) is
  pragma Unreferenced (F);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end Get_Line;

   --------------
   -- GET_LINE --
   --------------

  procedure Get_Line (B : out Boolean) is
  pragma Unreferenced (B);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end Get_Line;

   ---------
   -- PUT --
   ---------

  procedure Put (C : in  Character) is
  begin
    Ada.Text_IO.Put(C);
  end Put;

   ---------
   -- PUT --
   ---------

  procedure Put (I : in  Integer) is
  begin
    IIO.Put(I);
  end Put;

   ---------
   -- PUT --
   ---------

  procedure Put (I : in  Integer; W:  Width) is
  begin
    IIO.Put(I, W);
  end Put;

   ---------
   -- PUT --
   ---------

  procedure Put (F : in  Float) is
  begin
    FIO.Put(F);
  end Put;

   ---------
   -- PUT --
   ---------

  procedure Put (F : in  Float; W:  Width; D : Decimals) is
  begin
    FIO.Put(F,W,D);
  end Put;

   ---------
   -- PUT --
   ---------

  procedure Put (B : in  Boolean) is
  begin
    BIO.Put(B);
  end Put;

   ---------
   -- PUT --
   ---------

  procedure Put (S : in  String) is
  begin
    Ada.Text_IO.Put(S);
  end Put;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (C : in  Character) is
  begin
    Put(C); New_Line;
  end Put_Line;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (I : in  Integer) is
  begin
    Put(I); New_Line;
  end Put_Line;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (I : in  Integer; W:  Width) is
  begin
    Put(I,W); New_Line;
  end Put_Line;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (F : in  Float) is
  begin
    Put(F); New_Line;
  end Put_Line;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (F : in  Float; W:  Width; D : Decimals) is
  begin
    Put(F,W,D); New_Line;
  end Put_Line;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (B : in  Boolean) is
  begin
    Put(B); New_Line;
  end Put_Line;

   --------------
   -- PUT_LINE --
   --------------

  procedure Put_Line (S : in  String) is
  begin
    Ada.Text_IO.Put_Line(S);
  end Put_Line;

   --------------
   -- NEW_LINE --
   --------------

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
  pragma Unreferenced (S);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "WAIT unimplemented");
    raise Program_Error;
  end Wait;

   ------------
   -- SIGNAL --
   ------------

  procedure Signal (S : Semaphore) is
  pragma Unreferenced (S);
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "SIGNAL unimplemented");
    raise Program_Error;
  end Signal;

end HAC_Pack;
