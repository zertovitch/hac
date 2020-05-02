-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC.Compiler, HAC.Data;

with Ada.Text_IO,
     Ada.Integer_Text_IO;

package HAC.PCode.Interpreter is
  use HAC.Compiler;

  --  We provide an ad-hoc pipe of ALL console Text I/O, in case we
  --  use something else than a standard terminal / console.
  --  Ouch.
  --
  --  See LEA project for a specific non-trivial (windowed) implementation.
  --  See Interpret_on_Current_IO for a non-piped implementation (normal terminal).

  package Boolean_Text_IO is new Ada.Text_IO.Enumeration_IO (Boolean);
  package RIO is new Ada.Text_IO.Float_IO (HAC.Data.HAC_Float);

  generic
    with function End_Of_File_Console return Boolean;
    with function End_Of_Line_Console return Boolean;
    with procedure Get_Console (i: out Integer; Width : Ada.Text_IO.Field := 0);
    with procedure Get_Console (f: out HAC.Data.HAC_Float;   Width : Ada.Text_IO.Field := 0);
    with procedure Get_Console (c: out Character);
    with procedure Skip_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);
    --
    with procedure Put_Console (
      i     : Integer;
      Width : Ada.Text_IO.Field       := Ada.Integer_Text_IO.Default_Width;
      Base  : Ada.Text_IO.Number_Base := Ada.Integer_Text_IO.Default_Base);
    with procedure Put_Console (
      f    : HAC.Data.HAC_Float;
      Fore : Integer := RIO.Default_Fore;
      Aft  : Integer := RIO.Default_Aft;
      Exp  : Integer := RIO.Default_Exp
    );
    with procedure Put_Console (
      b     : in Boolean;
      Width : Ada.Text_IO.Field    := Boolean_Text_IO.Default_Width;
      Set   : Ada.Text_IO.Type_Set := Boolean_Text_IO.Default_Setting);
    with procedure Put_Console (c: in Character);
    with procedure Put_Console (s: in String);
    with procedure New_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);

    --  We also abstract the command-line parameters.
    --  Function profiles from Ada.Command_Line (RM A.15)
    with function Argument_Count return Natural;
    with function Argument (Number : in Positive) return String;

  procedure Interpret (CD: Compiler_Data);

  procedure Interpret_on_Current_IO (
    CD             : Compiler_Data;
    Argument_Shift : Natural := 0    --  Number of arguments to be skipped
  );

end HAC.PCode.Interpreter;
