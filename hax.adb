with HAC.Compiler,
     HAC.Data,
     HAC.PCode.Interpreter;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Text_IO;                       use Ada.Text_IO;

with GNAT.Traceback.Symbolic, Ada.Exceptions;

procedure HAX is

  verbosity : Natural := 0;
  caveat       : constant String := "Caution: HAC is not a real Ada compiler.";
  version_info : constant String :=
    "Version: " & HAC.version & " Reference: " & HAC.reference & '.';

  procedure Compile_and_interpret_file (name: String) is
    f : Ada.Streams.Stream_IO.File_Type;
    t1, t2 : Time;
    HAC_margin_1 : constant String := "*******[ HAX ]*******   ";
    HAC_margin_2 : constant String := ". . . .[ HAX ]. . . .   ";
    HAC_margin_3 : constant String := "-------[ HAX ]-------   ";
  begin
    case verbosity is
      when 0 =>
        null;
      when 1 =>
        Put_Line (HAC_margin_2 & "Compiling and running from file: " & name);
      when others =>
        New_Line;
        Put_Line (HAC_margin_1 & version_info);
        Put_Line (HAC_margin_1 & caveat);
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
      Put_Line(HAC_margin_3 & "Error: file not found (perhaps in exm or test subdirectory ?)");
  end Compile_and_interpret_file;

  procedure Help is
  begin
    Put_Line (Standard_Error, "HAX: command-line compilation and execution for HAC (HAC Ada Compiler)");
    Put_Line (Standard_Error, version_info);
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Usage: hax [options] main.adb [command-line parameters for main]");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, "Options: -h     : this help");
    Put_Line (Standard_Error, "         -v, v1 : verbose");
    Put_Line (Standard_Error, "         -v2    : very verbose");
    Put_Line (Standard_Error, "         -d     : debug information");
    New_Line (Standard_Error);
    Put_Line (Standard_Error, caveat);
  end Help;

begin
  if Argument_Count = 0 then
    Help;
  end if;
  HAC.Data.qDebug := False;
  for i in 1 .. Argument_Count loop
    if Argument (i) = "-h" then
      Help;
    elsif Argument (i) = "-v" or else Argument (i) = "-v1" then
      verbosity := 1;
    elsif Argument (i) = "-v2" then
      verbosity := 2;
    elsif Argument (i) = "-d" then
      HAC.Data.qDebug := True;
    else
      Compile_and_interpret_file (Argument (i));
    end if;
  end loop;
exception
  when E: others =>
    New_Line (Standard_Error);
    Put_Line (Standard_Error,
              "--------------------[ Unhandled exception ]-----------------");
    Put_Line (Standard_Error, " > Name of exception . . . . .: " &
              Ada.Exceptions.Exception_Name (E) );
    Put_Line (Standard_Error, " > Message for exception . . .: " &
              Ada.Exceptions.Exception_Message (E) );
    Put_Line (Standard_Error, " > Trace-back of call stack: " );
    Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E) );
end HAX;
