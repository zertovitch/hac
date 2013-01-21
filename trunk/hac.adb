with SmAda_Data;
with Compiler; use Compiler;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with GNAT.Traceback.Symbolic, Ada.Exceptions;
use Ada.Exceptions;

procedure HAC is

  procedure Compile_File(name: String) is
    f: Ada.Streams.Stream_IO.File_Type;
  begin
    Put_Line("Compiling " & name);
    Open(f, In_file, name);
    SmAda_Data.c_Set_Stream(SmAda_Data.Stream_Access(Stream(f)));
    Compile;
    Close(f);
  end Compile_File;

begin
  if Argument_Count = 0 then
    Compile_File("mini.sma");
  else
    Compile_File(Argument(1));
  end if;
exception
  when E: others =>
    New_Line(Standard_Error);
    Put_Line(Standard_Error,
             "--------------------[ Unhandled exception ]-----------------");
    Put_Line(Standard_Error, " > Name of exception . . . . .: " &
             Ada.Exceptions.Exception_Name(E) );
    Put_Line(Standard_Error, " > Message for exception . . .: " &
             Ada.Exceptions.Exception_Message(E) );
    Put_Line(Standard_Error, " > Trace-back of call stack: " );
    Put_Line(Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback(E) );
end HAC;
