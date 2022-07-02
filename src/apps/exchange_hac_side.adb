-----------------------------------------
--  HAC <-> Native data exchange demo  --
-----------------------------------------
--  HAC side                           --
-----------------------------------------

--  This file is meant to be compiled by the HAC compiler which is
--  embedded in Exchange_Native_Side (exchange_native_side.adb).

with HAL;

with Exchange_Common;

procedure Exchange_HAC_Side is

  use HAL;

  procedure Demo_Parameterless is
    procedure Parameterless_Callback with Import => True;
  begin
    Put_Line ("   HAC: I call a parameterless callback.");
    Parameterless_Callback;
    Put_Line ("   HAC: done calling.");
    New_Line;
  end Demo_Parameterless;

  procedure Demo_Data_to_Native is
    procedure Hello_Callback (message : in VString) with Import => True;
    procedure Ints_Callback (i, j, k : in Integer) with Import => True;
    procedure Floats_Callback (f, g, h : in Real) with Import => True;
  begin
    Put_Line ("   HAC: I send some stuff through callbacks.");
    Hello_Callback (+"I'm HAC and I say hello!");
    Ints_Callback (123, 456, 789);
    Floats_Callback (123.0, 456.0, 789.0);
    New_Line;
  end Demo_Data_to_Native;

  procedure Demo_Data_Bidirectional is
    procedure Hello_Callback_in_out (message : in out VString) with Import => True;
    procedure Ints_Callback_in_out (i : in out Integer) with Import => True;
    procedure Floats_Callback_in_out (f : in out Real) with Import => True;
    m : VString := +"I'm HAC";
    i : Integer := 12;
    f : Real := 11.0;
  begin
    m := +"I'm HAC";
    Put_Line ("   HAC: message before call: [" & m & ']');
    Hello_Callback_in_out (m);
    Put_Line ("   HAC: message after call: [" & m & ']');
    Put_Line (+"   HAC: integer before call: [" & i & ']');
    Ints_Callback_in_out (i);
    Put_Line (+"   HAC: integer after call: [" & i & ']');
    Put_Line (+"   HAC: float before call: [" & f & ']');
    Floats_Callback_in_out (f);
    Put_Line (+"   HAC: float after call: [" & f & ']');
    New_Line;
  end Demo_Data_Bidirectional;

  procedure Demo_Composite is
    type Matrix is array (1 .. 2, 1 .. 2) of HAL.Real;
    type Some_Record is record
      i : Integer;
      v : VString;
      e : Exchange_Common.Animal;
    end record;
    procedure Composite_Callback (a, b : in Matrix; r : in out Some_Record; c : out Matrix)
    with Import => True;
    m, n, o : Matrix;
    s : Some_Record;
  begin
    --  HAC 0.2: no aggregates, we have to write the matrix' contents cell by cell.
    m (1, 1) := 1.1;
    m (1, 2) := 1.2;
    m (2, 1) := 1.3;
    m (2, 2) := 1.4;
    n (1, 1) := -2.1;
    n (1, 2) :=  2.2;
    n (2, 1) := -2.3;
    n (2, 2) :=  2.4;
    s.i := 13;
    s.v := +"I'm a HAC record field";
    s.e := Exchange_Common.bat;
    Put_Line (+"   HAC: integer before call: [" & s.i & ']');
    Put_Line (+"        message before call: [" & s.v & ']');
    Put_Line (+"           enum before call: [" & s.e'Image & ']');
    Composite_Callback (m, n, s, o);
    Put_Line (+"   HAC: integer after call: [" & s.i & ']');
    Put_Line (+"        message after call: [" & s.v & ']');
    Put_Line (+"           enum after call: [" & s.e'Image & ']');
    Put_Line (+"        matrix product:");
    for i in o'Range (1) loop
      Put ("          ");
      for j in o'Range (2) loop
        Put (o (i, j), 2, 2, 0);
      end loop;
      New_Line;
    end loop;
  end Demo_Composite;
begin
  Put_Line ("   Exchange_HAC_Side is started.");
  New_Line;
  --
  Demo_Parameterless;
  Demo_Data_to_Native;
  Demo_Data_Bidirectional;
  Demo_Composite;
  --
  Set_VM_Variable ("Demo_Variable", Get_VM_Variable ("Demo_Variable") & "HAC... ");
end Exchange_HAC_Side;
