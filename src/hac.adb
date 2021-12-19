with HAC_Sys.Builder,
     HAC_Sys.PCode.Interpreter.In_Defs;

with Show_License;

with Ada.Calendar,
     Ada.Command_Line,
     Ada.Containers,
     Ada.Directories,
     Ada.Exceptions,
     Ada.Strings.Unbounded,
     Ada.Text_IO.Text_Streams;

procedure HAC is

  verbosity : Natural := 0;
  caveat       : constant String := "Caution: HAC is not a complete Ada compiler.";
  version_info : constant String :=
    "Compiler version: " & HAC_Sys.version & " dated " & HAC_Sys.reference & '.';

  HAC_margin_1 : constant String := "*******[ HAC ]*******   ";
  HAC_margin_2 : constant String := ". . . .[ HAC ]. . . .   ";
  HAC_margin_3 : constant String := "-------[ HAC ]-------   ";

  use Ada.Strings.Unbounded;

  procedure Compilation_Feedback (Message : String) is
    use Ada.Text_IO;
  begin
    case verbosity is
      when 0      => null;
      when 1      => Put_Line (Message);
      when others => Put_Line (HAC_margin_2 & Message);
    end case;
  end Compilation_Feedback;

  asm_dump_file_name : Unbounded_String;
  cmp_dump_file_name : Unbounded_String;

  procedure Compile_and_interpret_file (Ada_file_name : String; arg_pos : Positive) is
    use HAC_Sys.Builder,
        HAC_Sys.PCode.Interpreter;
    use Ada.Calendar, Ada.Command_Line, Ada.Containers, Ada.Text_IO;
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
    BD : Build_Data;
    post_mortem : Post_Mortem_Data;
    unhandled_found : Boolean;
    shebang_offset : Natural;
  begin
    if verbosity > 1 then
      New_Line;
      Put_Line (HAC_margin_1 & version_info);
      Put_Line (HAC_margin_1 & caveat & " Type ""hac"" for license.");
    end if;
    Open (f, In_File, Ada_file_name);
    Skip_Shebang (f, shebang_offset);
    Set_Diagnostic_File_Names (BD, To_String (asm_dump_file_name), To_String (cmp_dump_file_name));
    Set_Main_Source_Stream (BD, Text_Streams.Stream (f), Ada_file_name, shebang_offset);
    Set_Message_Feedbacks (BD, null, Unrestricted (Compilation_Feedback'Address));
    t1 := Clock;
    Build_Main (BD);
    t2 := Clock;
    Close (f);
    if verbosity >= 2 then
      Put_Line (
        HAC_margin_2 & "Build finished in" &
        Duration'Image (t2 - t1) &
        " seconds."
      );
    end if;
    --
    if not Build_Successful (BD) then
      Set_Exit_Status (Failure);
      return;
    end if;
    if verbosity >= 2 then
      Put_Line (HAC_margin_2 & "Object code size:" & Object_Code_Size (BD)'Image &
                " of" & Maximum_Object_Code_Size'Image &
                " Virtual Machine instructions.");
      Put_Line (HAC_margin_2 & "Starting p-code VM interpreter...");
    end if;
    t1 := Clock;
    Interpret_on_Current_IO (
      BD,
      arg_pos,
      Ada.Directories.Full_Name (Ada_file_name),
      post_mortem
    );
    t2 := Clock;
    unhandled_found := Is_Exception_Raised (post_mortem.Unhandled);
    if verbosity >= 2 then
      if unhandled_found then
        Put_Line (
          HAC_margin_3 & "VM interpreter stopped execution of " &
            Ada_file_name & " due to an unhandled exception.");
      else
        Put_Line (
          HAC_margin_3 & "VM interpreter done after" &
          Duration'Image (t2 - t1) & " seconds."
        );
      end if;
    end if;
    if unhandled_found then
      Put_Line (Current_Error, "HAC VM: raised " & Image (post_mortem.Unhandled));
      Put_Line (Current_Error, Message (post_mortem.Unhandled));
      Put_Line (Current_Error, "Trace-back: approximate location");
      CIO_Trace_Back (post_mortem.Unhandled);
      Set_Exit_Status (Failure);
    elsif verbosity >= 1 then
      Put_Line ("Execution of " & Ada_file_name & " completed.");
    end if;
    if verbosity >= 2 then
      Put_Line (
        "Maximum stack usage:" & post_mortem.Max_Stack_Usage'Image & " of" &
        post_mortem.Stack_Size'Image & " units, around" &
        Integer'Image (100 * post_mortem.Max_Stack_Usage / post_mortem.Stack_Size) & "%."
      );
    end if;
    if verbosity >= 1 then
      if post_mortem.Open_Files.Length > 0 then
        Put_Line ("List of files that were left open during execution:");
        for ofd of post_mortem.Open_Files loop
          Put_Line ("  Name: " & To_String (ofd.Name) & ", mode: " & File_Mode'Image (ofd.Mode));
        end loop;
      end if;
    end if;
  exception
    when E : Abnormal_Termination =>
      Put_Line (
        Current_Error,
        Ada.Exceptions.Exception_Message (E)
      );
      Set_Exit_Status (Failure);
    when Name_Error =>
      Put_Line (
        Current_Error,
        HAC_margin_3 &
        "Error: file """ & Ada_file_name &
        """ not found (perhaps in exm or test subdirectory ?)");
      Set_Exit_Status (Failure);
  end Compile_and_interpret_file;

  assembler_output_name : constant String := "asm_dump.pca";       --  PCA = PCode Assembler
  compiler_dump_name    : constant String := "compiler_dump.lst";

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
    Put_Line (Current_Error, "         -a     : assembler output in " & assembler_output_name);
    Put_Line (Current_Error, "         -d     : dump compiler information in " & compiler_dump_name);
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
      asm_dump_file_name := To_Unbounded_String (assembler_output_name);
    elsif Argument (i) = "-d" then
      cmp_dump_file_name := To_Unbounded_String (compiler_dump_name);
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
