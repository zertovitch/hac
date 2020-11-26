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

with Ada.Calendar, Ada.Containers.Vectors, Ada.Text_IO;

package HAC.PCode.Interpreter is

  use Co_Defs;

  ------------------
  --  Exceptions  --
  ------------------

  type Exception_Propagation_Data is private;

  function Is_Exception_Raised (E : Exception_Propagation_Data) return Boolean;

  function Image (E : Exception_Propagation_Data) return String;
  function Message (E : Exception_Propagation_Data) return String;

  generic
    with procedure Show_Line_Information (
      File_Name   : String;   --  Example: hac-pcode-interpreter.adb
      Block_Name  : String;   --  Example: HAC.PCode.Interpreter.Do_Write_Formatted
      Line_Number : Positive
    );
  procedure Show_Trace_Back (E : Exception_Propagation_Data);

  ------------------------------------------------------------------------------
  --  Here, we provide a ready-to-use, "standard" instantiation of the        --
  --  interpreter, with Ada.Text_IO, Ada.Command_Line, ..., for the console.  --
  --  See hax.adb for an example where it is used.                            --
  ------------------------------------------------------------------------------

  procedure Interpret_on_Current_IO (
    CD_CIO           :     Compiler_Data;
    Argument_Shift   :     Natural := 0;    --  Number of arguments to be skipped
    Full_Script_Name :     String;
    Unhandled        : out Exception_Propagation_Data
  );

  --  Part of the subprograms useed for the Interpret_on_Current_IO
  --  instanciation.
  --
  function Current_IO_Get_Needs_Skip_Line return Boolean;

  ---------------------------------------------------------------------------------
  --  The following version of the interpreter abstracts ALL console Text I/O,   --
  --  in case we use something else than a standard terminal / console.          --
  --  Similarily we abstract Argument_Count and a few others.                    --
  --  See the LEA project for a specific non-trivial (windowed) implementation.  --
  ---------------------------------------------------------------------------------

  --  Due to the large amount of abstracted subprograms, we wrap
  --  some groups into "traits". The idea is explained here:
  --  https://blog.adacore.com/traits-based-containers
  --
  generic
    with function End_Of_File return Boolean;
    with function End_Of_Line return Boolean;
    with function Get_Needs_Skip_Line return Boolean;
    --  ^ True  for a real console with Ada.Text_IO (line buffer);
    --    False for input boxes (like in LEA) or other kind of immediate input.
    with procedure Get (I : out Integer; Width : Ada.Text_IO.Field := 0);
    with procedure Get (F : out HAC.Defs.HAC_Float; Width : Ada.Text_IO.Field := 0);
    with procedure Get (C : out Character);
    with procedure Get_Immediate (C : out Character);
    with function Get_Line return String;
    with procedure Skip_Line (Spacing : Ada.Text_IO.Positive_Count := 1);
    --
    with procedure Put (
      I     : HAC.Defs.HAC_Integer;
      Width : Ada.Text_IO.Field       := HAC.Defs.IIO.Default_Width;
      Base  : Ada.Text_IO.Number_Base := HAC.Defs.IIO.Default_Base);
    with procedure Put (
      F    : HAC.Defs.HAC_Float;
      Fore : Integer := HAC.Defs.RIO.Default_Fore;
      Aft  : Integer := HAC.Defs.RIO.Default_Aft;
      Exp  : Integer := HAC.Defs.RIO.Default_Exp
    );
    with procedure Put (
      B     : in Boolean;
      Width : Ada.Text_IO.Field    := HAC.Defs.BIO.Default_Width;
      Set   : Ada.Text_IO.Type_Set := HAC.Defs.BIO.Default_Setting);
    with procedure Put (C : in Character);
    with procedure Put (S : in String);
    with procedure New_Line (Spacing : Ada.Text_IO.Positive_Count := 1);
  package Console_Traits is
  end Console_Traits;

  generic
    --  Function profiles for Argument* are from Ada.Command_Line (RM A.15).
    with function Argument_Count return Natural;
    with function Argument (Number : in Positive) return String;
    with function Command_Name return String;
    --  Shell execution, Directory_Separator, ... are also abstracted.
    with function Shell_Execute (Command : String) return Integer;
    with function Directory_Separator return Character;
  package System_Calls_Traits is
  end System_Calls_Traits;

  generic
    with procedure Feedback (
      Stack_Current, Stack_Total : in     Natural;
      Wall_Clock                 : in     Ada.Calendar.Time;
      User_Abort                 :    out Boolean
    );
    with package Console is new Console_Traits (<>);
    with package System_Calls is new System_Calls_Traits (<>);
    --
  procedure Interpret (CD : Compiler_Data; Unhandled : out Exception_Propagation_Data);

  Abnormal_Termination : exception;

private

  type Exception_Type is
    (No_Exception,
     --  Ada classics:
     VME_Constraint_Error,
     VME_Data_Error,
     VME_Program_Error,
     VME_End_Error,
     VME_Name_Error,
     VME_Use_Error,
     VME_Status_Error,
     VME_Storage_Error,
     --
     VME_User_Abort,
     VME_Custom
    );

  subtype Exception_Detail is Integer;
  --  Currently a placeholder (this is for the VME_Custom choice)

  type Exception_Identity is record
    Ex_Typ : Exception_Type;
    Detail : Integer;  --  For the VME_Custom choice
  end record;

  package Stack_Trace_Messages is new Ada.Containers.Vectors (Positive, Debug_Info);
  subtype Stack_Trace_Message is Stack_Trace_Messages.Vector;

  type Exception_Propagation_Data is record
    Currently_Raised  : Exception_Identity;
    ST_Message        : Stack_Trace_Message;
    Exception_Message : Defs.VString;
  end record;

end HAC.PCode.Interpreter;
