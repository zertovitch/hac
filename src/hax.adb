with HAC.Compiler,
     HAC.Co_Defs,
     HAC.PCode.Interpreter;

with Show_License;

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Streams.Stream_IO;             use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;

with GNAT.Traceback.Symbolic, Ada.Exceptions;

procedure HAX is

  verbosity : Natural := 0;
  caveat       : constant String := "Caution: HAC is not a real Ada compiler.";
  version_info : constant String :=
    "Compiler version: " & HAC.version & " dated " & HAC.reference & '.';

  asm_dump_file_name : Unbounded_String;
  cmp_dump_file_name : Unbounded_String;

  procedure Compile_and_interpret_file (Ada_file_name: String; arg_pos : Positive) is
    procedure Show_Line_Information (
      File_Name  : String;   --  Example: hac-pcode-interpreter.adb
      Block_Name : String;   --  Example: HAC.PCode.Interpreter.Do_Write_Formatted
      Number     : Positive
    )
    is
    begin
      Put_Line (Current_Error, File_Name & ": " & Block_Name & ':' & Integer'Image (Number));
    end Show_Line_Information;
    --
    use HAC.Compiler, HAC.Co_Defs, HAC.PCode.Interpreter;
    --
    procedure CIO_Trace_Back is new Show_Trace_Back (Show_Line_Information);
    --
    f : Ada.Streams.Stream_IO.File_Type;
    t1, t2 : Time;
    HAC_margin_1 : constant String := "*******[ HAX ]*******   ";
    HAC_margin_2 : constant String := ". . . .[ HAX ]. . . .   ";
    HAC_margin_3 : constant String := "-------[ HAX ]-------   ";
    CD : Compiler_Data;
    unhandled : Exception_Propagation_Data;
    unhandled_found : Boolean;
  begin
    case verbosity is
      when 0 =>
        null;
      when 1 =>
        Put_Line (HAC_margin_2 & "Compiling and running from file: " & Ada_file_name);
      when others =>
        New_Line;
        Put_Line (HAC_margin_1 & version_info);
        Put_Line (HAC_margin_1 & caveat & " Type ""hax"" for license.");
        Put_Line (HAC_margin_2 & "Compiling from file: " & Ada_file_name);
    end case;
    Open (f, In_File, Ada_file_name);
    Set_Source_Stream (CD, Stream(f), Ada_file_name);
    t1 := Clock;
    Compile (
      CD,
      To_String (asm_dump_file_name),
      To_String (cmp_dump_file_name)
    );
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
    if Unit_Compilation_Successful (CD) then
      if verbosity >= 2 then
        Put_Line (HAC_margin_2 & "Starting p-code VM interpreter...");
      end if;
      t1 := Clock;
      Interpret_on_Current_IO (CD, arg_pos, unhandled);
      t2 := Clock;
      unhandled_found := Is_in_Exception (unhandled);
      if verbosity >= 2 then
        if unhandled_found then
        Put_Line (
          HAC_margin_3 & "VM interpreter stopped execution of " &
            Ada_file_name & " due to an unhandled exception.");
        else
        Put_Line (
          HAC_margin_3 & "VM interpreter done after " &
          (Duration'Image(t2-t1)) & " seconds."
        );
        Put_Line (
          HAC_margin_3 & "Execution of " & Ada_file_name & " completed.");
        end if;
      end if;
      if unhandled_found then
        Put_Line (Current_Error, "HAC VM: raised " & Image (unhandled));
        Put_Line (Current_Error, Message (unhandled));
        Put_Line (Current_Error, "Trace-back locations:");
        CIO_Trace_Back (unhandled);
      end if;
    end if;
  exception
    when E: Abnormal_Termination =>
      Put_Line (
        Current_Error,
        Ada.Exceptions.Exception_Message (E)
      );
    when Ada.Streams.Stream_IO.Name_Error =>
      Put_Line (
        Current_Error,
        HAC_margin_3 &
        "Error: file """ & Ada_file_name &
        """ not found (perhaps in exm or test subdirectory ?)");
  end Compile_and_interpret_file;

  procedure Help is
  begin
    Put_Line (Current_Error, "HAX: command-line compilation and execution for HAC (HAC Ada Compiler)");
    Put_Line (Current_Error, version_info);
    Put_Line (Current_Error, "URL: " & HAC.web);
    New_Line (Current_Error);
    Put_Line (Current_Error, "Usage: hax [options] main.adb [command-line parameters for main]");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Options: -h     : this help");
    Put_Line (Current_Error, "         -v, v1 : verbose");
    Put_Line (Current_Error, "         -v2    : very verbose");
    Put_Line (Current_Error, "         -a     : assembler output");
    Put_Line (Current_Error, "         -d     : dump compiler information");
    New_Line (Current_Error);
    Put_Line (Current_Error, caveat);
    Show_License (Current_Error, "hac.ads");
  end Help;

begin
  if Argument_Count = 0 then
    Help;
  end if;
  for i in 1 .. Argument_Count loop
    if Argument (i) = "-h" then
      Help;
    elsif Argument (i) = "-v" or else Argument (i) = "-v1" then
      verbosity := 1;
    elsif Argument (i) = "-v2" then
      verbosity := 2;
    elsif Argument (i) = "-a" then
      asm_dump_file_name := To_Unbounded_String ("dump.pca");  --  PCA = PCode Assembler
    elsif Argument (i) = "-d" then
      cmp_dump_file_name := To_Unbounded_String ("symbols.lst");
    else
      Compile_and_interpret_file (Argument (i), i);
      exit;
    end if;
  end loop;
exception
  when E: others =>
    New_Line (Current_Error);
    Put_Line (Current_Error,
              "--------------------[ Unhandled exception ]-----------------");
    Put_Line (Current_Error, " > Name of exception . . . . .: " &
              Ada.Exceptions.Exception_Name (E) );
    Put_Line (Current_Error, " > Message for exception . . .: " &
              Ada.Exceptions.Exception_Message (E) );
    Put_Line (Current_Error, " > Trace-back of call stack: " );
    Put_Line (Current_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E) );
end HAX;
