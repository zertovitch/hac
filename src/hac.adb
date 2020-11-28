with HAC_Sys.Compiler,
     HAC_Sys.Co_Defs,
     HAC_Sys.PCode.Interpreter.In_Defs;

with Show_License;

with Ada.Calendar,
     Ada.Command_Line,
     Ada.Directories,
     Ada.Exceptions,
     Ada.Strings.Unbounded,
     Ada.Text_IO.Text_Streams;

procedure HAC is

  verbosity : Natural := 0;
  caveat       : constant String := "Caution: HAC is not a complete Ada compiler.";
  version_info : constant String :=
    "Compiler version: " & HAC_Sys.version & " dated " & HAC_Sys.reference & '.';

  use Ada.Strings.Unbounded;

  asm_dump_file_name : Unbounded_String;
  cmp_dump_file_name : Unbounded_String;

  procedure Compile_and_interpret_file (Ada_file_name : String; arg_pos : Positive) is
    use HAC_Sys.Compiler, HAC_Sys.Co_Defs, HAC_Sys.PCode.Interpreter,
        Ada.Calendar, Ada.Text_IO;
    --
    procedure Show_Line_Information (
      File_Name   : String;   --  Example: hac-pcode-interpreter.adb
      Block_Name  : String;   --  Example: HAC.PCode.Interpreter.Do_Write_Formatted
      Line_Number : Positive
    )
    is
    begin
      Put_Line (
        Current_Error,
        File_Name & ": " &
        Block_Name & " at line" &
        Integer'Image (Line_Number)
      );
    end Show_Line_Information;
    --
    procedure CIO_Trace_Back is new Show_Trace_Back (Show_Line_Information);
    --
    f : File_Type;
    t1, t2 : Time;
    HAC_margin_1 : constant String := "*******[ HAC ]*******   ";
    HAC_margin_2 : constant String := ". . . .[ HAC ]. . . .   ";
    HAC_margin_3 : constant String := "-------[ HAC ]-------   ";
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
        Put_Line (HAC_margin_1 & caveat & " Type ""hac"" for license.");
        Put_Line (HAC_margin_2 & "Compiling from file: " & Ada_file_name);
    end case;
    Open (f, In_File, Ada_file_name);
    --
    --  Skip an eventual "shebang", e.g.: #!/usr/bin/env hac
    --  The Ada source begins from next line.
    --
    if not End_Of_File (f) then
      declare
        possible_shebang : constant String := Get_Line (f);
      begin
        if possible_shebang'Length > 1
          and then possible_shebang (possible_shebang'First .. possible_shebang'First + 1) = "#!"
        then
          null;  --  Just ignore the first line.
        else
          Reset (f);
        end if;
      end;
    end if;
    Set_Source_Stream (CD, Text_Streams.Stream (f), Ada_file_name);
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
        (Duration'Image (t2 - t1)) &
        " seconds."
      );
    end if;
    --
    if Unit_Compilation_Successful (CD) then
      if verbosity >= 2 then
        Put_Line (HAC_margin_2 & "Starting p-code VM interpreter...");
      end if;
      t1 := Clock;
      Interpret_on_Current_IO (CD, arg_pos, Ada.Directories.Full_Name (Ada_file_name), unhandled);
      t2 := Clock;
      unhandled_found := Is_Exception_Raised (unhandled);
      if verbosity >= 2 then
        if unhandled_found then
          Put_Line (
            HAC_margin_3 & "VM interpreter stopped execution of " &
              Ada_file_name & " due to an unhandled exception.");
        else
          Put_Line (
            HAC_margin_3 & "VM interpreter done after " &
            (Duration'Image (t2 - t1)) & " seconds."
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
    when E : Abnormal_Termination =>
      Put_Line (
        Current_Error,
        Ada.Exceptions.Exception_Message (E)
      );
    when Name_Error =>
      Put_Line (
        Current_Error,
        HAC_margin_3 &
        "Error: file """ & Ada_file_name &
        """ not found (perhaps in exm or test subdirectory ?)");
  end Compile_and_interpret_file;

  procedure Help is
    use Ada.Text_IO;
  begin
    Put_Line (Current_Error, "HAC: command-line compilation and execution for HAC (HAC Ada Compiler)");
    Put_Line (Current_Error, version_info);
    Put_Line (Current_Error, "URL: " & HAC_Sys.web);
    New_Line (Current_Error);
    Put_Line (Current_Error, "Usage: hac [options] main.adb [command-line parameters for main]");
    New_Line (Current_Error);
    Put_Line (Current_Error, "Options: -h     : this help");
    Put_Line (Current_Error, "         -v, v1 : verbose");
    Put_Line (Current_Error, "         -v2    : very verbose");
    Put_Line (Current_Error, "         -a     : assembler output");
    Put_Line (Current_Error, "         -d     : dump compiler information");
    New_Line (Current_Error);
    Put_Line (Current_Error, caveat);
    Put_Line (Current_Error, "Note: HAC (this command-line tool) accepts source files with shebang's,");
    Put_Line (Current_Error, "      for instance:   #!/usr/bin/env hac     or     #!/usr/bin/hac");
    Show_License (Current_Error, "hac_sys.ads");
  end Help;

  no_hac_ing : Boolean := True;
  use Ada.Command_Line;

begin
  for i in 1 .. Argument_Count loop
    if Argument (i) = "-h" then
      exit;
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
      no_hac_ing := False;
      exit;  --  The other arguments are for the HAC program.
    end if;
  end loop;
  if no_hac_ing then
    Help;
    if verbosity > 1 then
      Ada.Text_IO.Put_Line ("Size of a HAC VM memory unit:" &
        Integer'Image (HAC_Sys.PCode.Interpreter.In_Defs.Data_Type'Size / 8) &
        " bytes"
      );
    end if;
  end if;
end HAC;
