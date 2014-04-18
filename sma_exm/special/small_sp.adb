with Ada.Numerics.Float_Random;         use Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

package body SMALL_SP is

  package FEF is new Ada.Numerics.Generic_Elementary_Functions(Float);

  function SQR (I : INTEGER) return INTEGER is
  begin
    return I*I;
  end SQR;

  function SQR (F : FLOAT) return FLOAT is
  begin
    return F*F;
  end SQR;

  function SQRT (I : INTEGER) return FLOAT is
  begin
    return FEF.Sqrt(Float(I));
  end SQRT;

  function SQRT (F : FLOAT) return FLOAT is
  begin
    return FEF.Sqrt(F);
  end SQRT;

  function ODD (I : INTEGER) return BOOLEAN is
  begin
    return I mod 2 = 1;
  end ODD;

  function ASCII (I : INTEGER) return CHARACTER is
  begin
    return Character'Val(I);
  end ASCII;

  function ORD (C : CHARACTER) return INTEGER is
  begin
    return Character'Pos(C);
  end ORD;

  function SUCC (C : CHARACTER) return CHARACTER is
  begin
    return Character'Succ (C);
  end SUCC;

  function PRED (C : CHARACTER) return CHARACTER is
  begin
    return Character'Pred (C);
  end PRED;

  function ROUND (F : FLOAT) return INTEGER is
  begin
    return Integer(F);
  end ROUND;

  function TRUNC (F : FLOAT) return INTEGER is
  begin
    return Integer(Float'Floor(F));
  end TRUNC;

  function SIN (I : INTEGER) return FLOAT is
  begin
    return FEF.Sin(Float(I));
  end SIN;

  function SIN (F : FLOAT) return FLOAT is
  begin
    return FEF.Sin(F);
  end SIN;

  function COS (I : INTEGER) return FLOAT is
  begin
    return FEF.Cos(Float(I));
  end COS;

  function COS (F : FLOAT) return FLOAT is
  begin
    return FEF.Cos(F);
  end COS;

  function ARCTAN (I : INTEGER) return FLOAT is
  begin
    return FEF.Arctan(Float(I));
  end ARCTAN;

  function ARCTAN (F : FLOAT) return FLOAT is
  begin
    return FEF.Arctan(F);
  end ARCTAN;

  function LN (I : INTEGER) return FLOAT is
  begin
    return FEF.Log(Float(I));
  end LN;

  function LN (F : FLOAT) return FLOAT is
  begin
    return FEF.Log(F);
  end LN;

  function EXP (I : INTEGER) return FLOAT is
  begin
    return FEF.Exp(Float(I));
  end EXP;

  function EXP (F : FLOAT) return FLOAT is
  begin
    return FEF.Exp(F);
  end EXP;

  gen: Generator;

  function RANDOM (I : INTEGER) return INTEGER is
  begin
    return Trunc(Random(I));
  end RANDOM;

  function RANDOM (I : INTEGER) return Float is
  begin
    return Random(gen) * Float(I);
  end RANDOM;

  package IIO is new Ada.Text_IO.Integer_IO(Integer);
  package FIO is new Ada.Text_IO.Float_IO(Float);
  package BIO is new Ada.Text_IO.Enumeration_IO(Boolean);

   ---------
   -- GET --
   ---------

  procedure GET (C : OUT CHARACTER) is
  begin
    Ada.Text_IO.Get(C);
  end GET;

   ---------
   -- GET --
   ---------

  procedure GET (I : OUT INTEGER) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET unimplemented");
    raise Program_Error;
  end GET;

   ---------
   -- GET --
   ---------

  procedure GET (F : OUT FLOAT) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET unimplemented");
    raise Program_Error;
  end GET;

   ---------
   -- GET --
   ---------

  procedure GET (B : OUT BOOLEAN) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET unimplemented");
    raise Program_Error;
  end GET;

   --------------
   -- GET_LINE --
   --------------

  procedure GET_LINE (C : OUT CHARACTER) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end GET_LINE;

   --------------
   -- GET_LINE --
   --------------

  procedure GET_LINE (I : OUT INTEGER) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end GET_LINE;

   --------------
   -- GET_LINE --
   --------------

  procedure GET_LINE (F : OUT FLOAT) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end GET_LINE;

   --------------
   -- GET_LINE --
   --------------

  procedure GET_LINE (B : OUT BOOLEAN) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "GET_LINE unimplemented");
    raise Program_Error;
  end GET_LINE;

   ---------
   -- PUT --
   ---------

  procedure PUT (C : IN  CHARACTER) is
  begin
    Ada.Text_IO.Put(C);
  end PUT;

   ---------
   -- PUT --
   ---------

  procedure PUT (I : IN  INTEGER) is
  begin
    IIO.Put(I);
  end PUT;

   ---------
   -- PUT --
   ---------

  procedure PUT (I : IN  INTEGER; W:  WIDTH) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "PUT unimplemented");
    raise Program_Error;
  end PUT;

   ---------
   -- PUT --
   ---------

  procedure PUT (F : IN  FLOAT) is
  begin
    FIO.Put(F);
  end PUT;

   ---------
   -- PUT --
   ---------

  procedure PUT (F : IN  FLOAT; W:  WIDTH; D : DECIMALS) is
  begin
    FIO.Put(F,W,D);
  end PUT;

   ---------
   -- PUT --
   ---------

  procedure PUT (B : IN  BOOLEAN) is
  begin
    BIO.Put(B);
  end PUT;

   ---------
   -- PUT --
   ---------

  procedure PUT (S : IN  String) is
  begin
    Ada.Text_IO.Put(s);
  end PUT;

   --------------
   -- PUT_LINE --
   --------------

  procedure PUT_LINE (C : IN  CHARACTER) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "PUT_LINE unimplemented");
    raise Program_Error;
  end PUT_LINE;

   --------------
   -- PUT_LINE --
   --------------

  procedure PUT_LINE (I : IN  INTEGER) is
  begin
    Put(I); New_Line;
  end PUT_LINE;

   --------------
   -- PUT_LINE --
   --------------

  procedure PUT_LINE (I : IN  INTEGER; W:  WIDTH) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "PUT_LINE unimplemented");
    raise Program_Error;
  end PUT_LINE;

   --------------
   -- PUT_LINE --
   --------------

  procedure PUT_LINE (F : IN  FLOAT) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "PUT_LINE unimplemented");
    raise Program_Error;
  end PUT_LINE;

   --------------
   -- PUT_LINE --
   --------------

  procedure PUT_LINE (F : IN  FLOAT; W:  WIDTH; D : DECIMALS) is
  begin
    Put(F,W,D); New_Line;
  end PUT_LINE;

   --------------
   -- PUT_LINE --
   --------------

  procedure PUT_LINE (B : IN  BOOLEAN) is
  begin
    Put(B); New_Line;
  end PUT_LINE;

   --------------
   -- PUT_LINE --
   --------------

  procedure PUT_LINE (S : IN  String) is
  begin
    Ada.Text_IO.Put_Line(s);
  end PUT_LINE;

   --------------
   -- NEW_LINE --
   --------------

  procedure NEW_LINE is
  begin
    Ada.Text_IO.New_Line;
  end NEW_LINE;

  procedure  CURSORAT (X, Y: Integer) is
  begin
    null; -- !!
  end;

   ----------
   -- WAIT --
   ----------

  procedure WAIT (S : SEMAPHORE) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "WAIT unimplemented");
    raise Program_Error;
  end WAIT;

   ------------
   -- SIGNAL --
   ------------

  procedure SIGNAL (S : SEMAPHORE) is
  begin
      --  Generated stub: replace with real body!
    pragma Compile_Time_Warning (Standard.True, "SIGNAL unimplemented");
    raise Program_Error;
  end SIGNAL;

end SMALL_SP;
