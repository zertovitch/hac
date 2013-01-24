with HAC.Data;
with HAC.Compiler;                      use HAC.Compiler;
with HAC.PCode.Interpreter;

with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;
with GNAT.Traceback.Symbolic, Ada.Exceptions;
use Ada.Exceptions;

procedure HAC_Test is

  procedure Compile_File(name: String) is
    f: Ada.Streams.Stream_IO.File_Type;
  begin
    Put_Line("Compiling " & name);
    Open(f, In_file, name);
    HAC.Data.c_Set_Stream(HAC.Data.Stream_Access(Stream(f)));
    Compile;
    Close(f);
    --
    HAC.PCode.Interpreter.Interpret;
  end Compile_File;

begin
  if Argument_Count = 0 then
    Compile_File("test.adb");
  else
    for i in 1..Argument_Count loop
      Compile_File(Argument(i));
    end loop;
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
end HAC_Test;
