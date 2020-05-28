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

with HAC.Co_Defs, HAC.Defs;

with Ada.Text_IO;

package HAC.PCode.Interpreter is

  use Co_Defs;

  ------------------------------------------------------------------------------
  --  Here, we provide a ready-to-use, "standard" instantiation of the        --
  --  interpreter, with Ada.Text_IO, Ada.Command_Line, ..., for the console.  --
  --  See hax.adb for an example where it is used.                            --
  ------------------------------------------------------------------------------

  procedure Interpret_on_Current_IO (
    CD_CIO         : Compiler_Data;
    Argument_Shift : Natural := 0    --  Number of arguments to be skipped
  );

  ----------------------------------------------------------------------------------
  --  This version of the interpreter abstracts ALL console Text I/O, in case we  --
  --  use something else than a standard terminal / console. Same for             --
  --  Argument_Count and a few others.                                            --
  --  See the LEA project for a specific non-trivial (windowed) implementation.   --
  ----------------------------------------------------------------------------------

  generic
    with function End_Of_File_Console return Boolean;
    with function End_Of_Line_Console return Boolean;
    with function Get_Needs_Skip_Line return Boolean;
    --  ^ True  for a real console with Ada.Text_IO (line buffer);
    --    False for input boxes (like in LEA) or other kind of immediate input.
    with procedure Get_Console (i: out Integer; Width : Ada.Text_IO.Field := 0);
    with procedure Get_Console (f: out HAC.Defs.HAC_Float; Width : Ada.Text_IO.Field := 0);
    with procedure Get_Console (c: out Character);
    with procedure Get_Immediate_Console (c: out Character);
    with function Get_Line_Console return String;
    with procedure Skip_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);
    --
    with procedure Put_Console (
      i     : HAC.Defs.HAC_Integer;
      Width : Ada.Text_IO.Field       := HAC.Defs.IIO.Default_Width;
      Base  : Ada.Text_IO.Number_Base := HAC.Defs.IIO.Default_Base);
    with procedure Put_Console (
      f    : HAC.Defs.HAC_Float;
      Fore : Integer := HAC.Defs.RIO.Default_Fore;
      Aft  : Integer := HAC.Defs.RIO.Default_Aft;
      Exp  : Integer := HAC.Defs.RIO.Default_Exp
    );
    with procedure Put_Console (
      b     : in Boolean;
      Width : Ada.Text_IO.Field    := HAC.Defs.BIO.Default_Width;
      Set   : Ada.Text_IO.Type_Set := HAC.Defs.BIO.Default_Setting);
    with procedure Put_Console (c: in Character);
    with procedure Put_Console (s: in String);
    with procedure New_Line_Console (Spacing : Ada.Text_IO.Positive_Count := 1);

    --  We also abstract the command-line parameters.
    --  Function profiles are from Ada.Command_Line (RM A.15).
    with function Argument_Count return Natural;
    with function Argument (Number : in Positive) return String;
    --  Shell execution, Directory_Separator, ... are also abstracted.
    with function Shell_Execute (Command : String) return Integer;
    with function Directory_Separator return Character;

  procedure Interpret (CD: Compiler_Data);

end HAC.PCode.Interpreter;
