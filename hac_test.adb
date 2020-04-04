with HAC.Data, HAC.Compiler, HAC.PCode.Interpreter;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;

with GNAT.Traceback.Symbolic, Ada.Exceptions;

procedure HAC_Test is

  verbosity : Natural := 2;

  procedure Compile_and_interpret_file (name: String) is
    f : Ada.Streams.Stream_IO.File_Type;
    t1, t2 : Time;
    HAC_margin_1 : constant String := "*******[ HAC ]*******   ";
    HAC_margin_2 : constant String := ". . . .[ HAC ]. . . .   ";
    HAC_margin_3 : constant String := "-------[ HAC ]-------   ";
  begin
    HAC.Data.qDebug := False;
    case verbosity is
      when 0 =>
        null;
      when 1 =>
        Put_Line (HAC_margin_2 & "Compiling and running from file: " & name);
      when others =>
        HAC.Data.qDebug := True;
        New_Line;
        Put_Line (HAC_margin_1 & "Caution: HAC is not a real Ada compiler.");
        Put_Line (HAC_margin_2 & "Compiling from file: " & name);
    end case;
    Open (f, In_File, name);
    HAC.Data.Line_Count:= 0;
    HAC.Data.c_Set_Stream (HAC.Data.Stream_Access(Stream(f)), name);
    t1 := Clock;
    HAC.Compiler.Compile;
    t2 := Clock;
    Close (f);
    if verbosity >= 2 then
      Put_Line (
        HAC_margin_2 & "Compilation finished in " &
        (Duration'Image(t2-t1)) &
        " seconds."
      );
    end if;
    --
    if HAC.Data.Err_Count = 0 then
      if verbosity >= 2 then
        Put_Line (HAC_margin_2 & "Starting p-code VM interpreter...");
      end if;
      t1 := Clock;
      HAC.PCode.Interpreter.Interpret_on_Current_IO;
      t2 := Clock;
      if verbosity >= 2 then
        Put_Line (
          HAC_margin_3 & "VM interpreter done after " &
          (Duration'Image(t2-t1)) &
          " seconds."
        );
      end if;
    end if;
  exception
    when Ada.Streams.Stream_IO.Name_Error =>
      Put_Line(HAC_margin_3 & "Error: file not found (perhaps in hac_exm or test subdirectory ?)");
  end Compile_and_interpret_file;

begin
  if Argument_Count = 0 then
    Compile_and_interpret_file ("test.adb");
  else
    for i in 1 .. Argument_Count loop
      if Argument (i) = "-q" then
        verbosity := 1;
      elsif Argument (i) = "-q2" then
        verbosity := 0;
      else
        Compile_and_interpret_file (Argument (i));
      end if;
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
