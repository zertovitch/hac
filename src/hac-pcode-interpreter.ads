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

with Ada.Containers.Vectors, Ada.Text_IO;

package HAC.PCode.Interpreter is

  use Co_Defs;

  ------------------
  --  Exceptions  --
  ------------------

  type Exception_Identity is private;
  function Is_in_Exception (E: Exception_Identity) return Boolean;
  function Image (E: Exception_Identity) return String;

  package Stack_Trace_Messages is
    new Ada.Containers.Vectors (Positive, Defs.VString, Defs.VStrings_Pkg."=");
  subtype Stack_Trace_Message is Stack_Trace_Messages.Vector;

  type Exception_Propagation_Data is record
    Currently_Raised  : Exception_Identity;
    ST_Message        : Stack_Trace_Message;
    Exception_Message : Defs.VString;
  end record;

  ------------------------------------------------------------------------------
  --  Here, we provide a ready-to-use, "standard" instantiation of the        --
  --  interpreter, with Ada.Text_IO, Ada.Command_Line, ..., for the console.  --
  --  See hax.adb for an example where it is used.                            --
  ------------------------------------------------------------------------------

  procedure Interpret_on_Current_IO (
    CD_CIO         :     Compiler_Data;
    Argument_Shift :     Natural := 0;    --  Number of arguments to be skipped
    Unhandled      : out Exception_Propagation_Data
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

  procedure Interpret (CD: Compiler_Data; Unhandled : out Exception_Propagation_Data);

  Abnormal_Termination : exception;

private

  type Exception_Type is
    (No_Exception,
     VME_Constraint_Error, VME_Program_Error, VME_End_Error, VME_Storage_Error,
     VME_Custom);

  subtype Exception_Detail is Integer;
  --  Currently a placeholder (this is for the VME_Custom choice)

  type Exception_Identity is record
    Ex_Typ : Exception_Type;
    Detail : Integer;  --  For the VME_Custom choice
  end record;

end HAC.PCode.Interpreter;
