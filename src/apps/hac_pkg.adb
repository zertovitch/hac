with HAC_Sys.Defs,
     HAC_Sys.PCode.Interpreter,
     HAC_Sys.Targets.AMD64_Windows_Console_FASM;

with Show_MIT_License;

with Ada.Command_Line,
     Ada.Containers,
     Ada.Directories,
     Ada.Exceptions,
     Ada.Text_IO;

package body HAC_Pkg is

  procedure Compilation_Feedback (message : String) is
  begin
    case verbosity is
      when 0      => null;
      when 1      => HAT.Put_Line (message);
      when others => HAT.Put_Line (HAC_margin_2 & message);
    end case;
  end Compilation_Feedback;

  package body Path_Management is

    overriding function Exists (cat : File_Catalogue; name : String) return Boolean is
    begin
      return cat.Full_Source_Name (name) /= "";
    end Exists;

    overriding function Full_Source_Name (cat : File_Catalogue; name : String) return String is
      --  Search order: same as GNAT's,
      --  cf. 4.2.2 Search Paths and the Run-Time Library (RTL).
      use Ada.Directories;
    begin
      --  0) The file name as such exists.
      if HAC_Sys.Files.Default.File_Catalogue (cat).Exists (name) then
        return name;
      end if;
      --  1) The directory containing the source file of the main unit
      --     being compiled (the file name on the command line).
      declare
        fn : constant String :=
          Ada.Directories.Containing_Directory (HAT.To_String (main_Ada_file_name)) &
          HAT.Directory_Separator &
          name;
      begin
        if Exists (fn) and then Kind (fn) = Ordinary_File then
          return fn;
        end if;
      exception
        when others => null;  --  Continue searching elsewhere.
      end;
      --  2) Each directory named by an -I switch given on the
      --     hac command line, in the order given.
      declare
        fn : constant String :=
          HAT.Search_File (name, HAT.To_String (command_line_source_path));
      begin
        if fn /= "" then
          return fn;
        end if;
      end;
      --  3) Omitted (directories listed in the text file whose name is given by ADA_PRJ_INCLUDE_FILE).
      --  4) Each of the directories listed in the value of the ADA_INCLUDE_PATH environment variable.
      declare
        fn : constant String :=
          HAT.Search_File (name, HAT.To_String (HAT.Get_Env ("ADA_INCLUDE_PATH")));
      begin
        if fn /= "" then
          return fn;
        end if;
      end;
      --  5) Omitted (content of the ada_source_path file).

      --  Now, extra search capabilities specific to HAC:
      declare
        fn : constant String :=
          HAT.Search_File (name, HAT.To_String (cat.extra_path));
      begin
        if fn /= "" then
          return fn;
        end if;
      end;

      return "";
    end Full_Source_Name;

    overriding function Is_Open (cat : File_Catalogue; name : String) return Boolean is
    begin
      return HAC_Sys.Files.Default.File_Catalogue (cat).Is_Open (cat.Full_Source_Name (name));
    end Is_Open;

    overriding procedure Source_Open
      (cat         : in out File_Catalogue;
       name        : in     String;
       stream      :    out HAC_Sys.Files.Root_Stream_Class_Access)
    is
      ffn : constant String := cat.Full_Source_Name (name);
    begin
      if ffn = "" then
        raise Ada.Directories.Name_Error;
      else
        HAC_Sys.Files.Default.File_Catalogue (cat).Source_Open (ffn, stream);
      end if;
    end Source_Open;

    overriding procedure Skip_Shebang
      (cat            : in out File_Catalogue;
       name           : in     String;
       shebang_offset :    out Natural) is
    begin
      HAC_Sys.Files.Default.File_Catalogue (cat).Skip_Shebang
        (cat.Full_Source_Name (name),
         shebang_offset);
    end Skip_Shebang;

    overriding procedure Close (cat : in out File_Catalogue; name : String) is
    begin
      HAC_Sys.Files.Default.File_Catalogue (cat).Close (cat.Full_Source_Name (name));
    end Close;

    overriding procedure Add_to_Source_Path (cat : in out File_Catalogue; new_dir : String) is
      use HAT;
    begin
      cat.extra_path := cat.extra_path & ';' & new_dir;
    end Add_to_Source_Path;

  end Path_Management;

  procedure PLCE (s : String) is
    use Ada.Text_IO;
  begin
    Put_Line (Current_Error, s);
  end PLCE;

  procedure Option_Head (s : String) is
  begin
    NLCE;
    PLCE ("______");
    PLCE ("Option " & s);
  end Option_Head;

  procedure NLCE is
    use Ada.Text_IO;
  begin
    New_Line (Current_Error);
  end NLCE;

  procedure Help (level : Positive) is
    use HAC_Sys.Defs, Ada.Text_IO;
    function Show_Level (r : Compile_Remark) return String is
      (" (from level" & Minimum_Level (r)'Image & ')');
  begin
    PLCE ("HAC: command-line build and execution tool for HAC (HAC Ada Compiler)");
    PLCE (version_info);
    PLCE ("Main URL: "           & HAC_Sys.web);
    PLCE ("  Sources, site #1: " & HAC_Sys.web2);
    PLCE ("  Sources, site #2: " & HAC_Sys.web3);
    PLCE ("  Alire Crate: "      & HAC_Sys.web4);
    NLCE;
    PLCE ("Usage: hac [options] main.adb [command-line parameters for main]");
    NLCE;
    PLCE ("Options: -h, h1 : this help");
    PLCE ("         -h2    : show more help & details about options");
    NLCE;
    PLCE ("         -a     : assembler output");
    PLCE ("         -c     : compile only");
    PLCE ("         -d     : dump compiler information in " & compiler_dump_name);
    PLCE ("         -I     : specify source files search path");
    PLCE ("         -rx    : enable / disable remarks");
    PLCE ("         -tx    : target machine (default: HAC VM)");
    PLCE ("         -v, v1 : verbose");
    PLCE ("         -v2    : very verbose");
    NLCE;
    PLCE ("Note: HAC (this command-line tool) accepts source files with shebang's,");
    PLCE ("      for instance:   #!/usr/bin/env hac     or     #!/usr/bin/hac");
    Show_MIT_License (Current_Error, "hac_sys.ads");
    if level > 1 then
      NLCE;
      PLCE ("/------------------------------------------\");
      PLCE ("| Extended help for HAC (command: hac -h2) |");
      PLCE ("\------------------------------------------/");
      Option_Head ("-I : specify source files search path");
      NLCE;
      PLCE ("  The search path is a list of directories separated by commas (,) or semicolons (;).");
      PLCE ("  HAC searches Ada source files in the following order:");
      PLCE ("    1) The directory containing the source file of the main unit");
      PLCE ("         being compiled (the file name on the command line).");
      PLCE ("    2) Each directory named by an -I switch given on the");
      PLCE ("         hac command line, in the order given.");
      PLCE ("    3) Each of the directories listed in the value of the ADA_INCLUDE_PATH");
      PLCE ("         environment variable.");
      Option_Head ("-rx : enable remarks (warnings or notes) of kind x");
      PLCE ("       -rX : disable remarks for letter x");
      PLCE ("             x =");
      PLCE
        ("                 0 .. 3 : enable remarks of level x; default is" &
         default_remark_level'Image);
      PLCE
        ("                 k :  notes for constant variables" &
         Show_Level (note_constant_variable));
      PLCE
        ("                 r :  notes for redundant constructs" &
         Show_Level (note_redundant_construct));
      PLCE
        ("                 u :  notes for unused items" &
         Show_Level (note_unused_item));
      PLCE
        ("                 v :  warnings for uninitialized variables or parameters" &
         Show_Level (warn_read_but_not_written));
      Option_Head ("-tx : set target machine to x");
      PLCE ("             x =");
      PLCE ("                 amd64_windows_console_fasm");
      NLCE;
    end if;
    Ada.Text_IO.Put ("Press Return");
    Ada.Text_IO.Skip_Line;
  end Help;

  type Target_List is
    (hac_vm, amd64_windows_console_fasm);

  target_choice : Target_List := hac_vm;

  procedure Set_Target (name : String) is
  begin
    target_choice := Target_List'Value (name);
    case target_choice is
      when hac_vm =>
        null;  --  Actual target in BD.CD is already initialized.
      when amd64_windows_console_fasm =>
        target := new HAC_Sys.Targets.AMD64_Windows_Console_FASM.Machine;
    end case;
  end Set_Target;

  procedure Failure is
    use HAT;
  begin
    if Ends_With (main_Ada_file_name, ".hac") then
      --  Main has the "HAC script extension", possibly run
      --  from Explorer, Nautilus, etc.
      Put ("Failure in " & main_Ada_file_name & ", press Return");
      Skip_Line;
    end if;
    Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
  end Failure;

  procedure Run_HAC_VM (BD : in out HAC_Sys.Builder.Build_Data; arg_pos : Positive) is
    use HAC_Sys.PCode.Interpreter, HAT;
    use Ada.Containers;
    --
    procedure Show_Line_Information (
      File_Name   : String;   --  Example: hac-pcode-interpreter.adb
      Block_Name  : String;   --  Example: HAC.PCode.Interpreter.Do_Write_Formatted
      Line_Number : Positive
    )
    is
    begin
      PLCE
        (File_Name & ": " &
         Block_Name & " at line" &
         Integer'Image (Line_Number));
    end Show_Line_Information;
    --
    procedure CIO_Trace_Back is new Show_Trace_Back (Show_Line_Information);
    --
    post_mortem     : Post_Mortem_Data;
    unhandled_found : Boolean;
    t1, t2          : Time;
  begin
    if verbosity >= 1 then
      New_Line;
    end if;
    t1 := Clock;
    Interpret_on_Current_IO
      (BD,
       arg_pos,
       Ada.Directories.Full_Name (To_String (main_Ada_file_name)),
       post_mortem);
    t2 := Clock;
    unhandled_found := Is_Exception_Raised (post_mortem.Unhandled);
    if verbosity >= 2 then
      --  The "if expression" commented out here confuses ObjectAda 10.4.
      --
      --  Put_Line
      --    (HAC_margin_3 &
      --       (if unhandled_found then
      --          "VM interpreter stopped execution of " &
      --          Ada_file_name & " due to an unhandled exception."
      --        else
      --          "VM interpreter done after" & Duration'Image (t2 - t1) & " seconds."));
      --
      if unhandled_found then
        Put_Line (
          HAC_margin_3 & "VM interpreter stopped execution of " &
            main_Ada_file_name & " due to an unhandled exception.");
      else
        Put_Line (
          HAC_margin_3 & "VM interpreter done after" &
          Duration'Image (t2 - t1) & " seconds."
        );
      end if;
    end if;
    if unhandled_found then
      PLCE ("HAC VM: raised " & Image (post_mortem.Unhandled));
      PLCE (Message (post_mortem.Unhandled));
      PLCE ("Trace-back: approximate location");
      CIO_Trace_Back (post_mortem.Unhandled);
      Failure;
    elsif verbosity >= 1 then
      Put_Line ("Execution of " & main_Ada_file_name & " completed.");
    end if;
    if verbosity >= 2 then
      Put_Line (
        "Maximum stack usage:" &
        Integer'Image (post_mortem.Max_Stack_Usage) & " of" &
        Integer'Image (post_mortem.Stack_Size) & " memory units, around" &
        Integer'Image (100 * post_mortem.Max_Stack_Usage / post_mortem.Stack_Size) & "%."
      );
    end if;
    if verbosity >= 1 then
      if post_mortem.Open_Files.Length > 0 then
        Put_Line ("List of files that were left open during execution:");
        for ofd of post_mortem.Open_Files loop
          Put_Line
           ("  Name: " & HAT.To_String (ofd.Name) &
            ", mode: " & Ada.Text_IO.File_Mode'Image (ofd.Mode));
        end loop;
      end if;
    end if;
  exception
    when E : Abnormal_Termination =>
      PLCE ("Abnormal Termination (VM): " & Ada.Exceptions.Exception_Message (E));
      Failure;
    when Ada.Text_IO.Name_Error =>
      PLCE
        (HAC_margin_3 &
         "Error: file """ & To_String (main_Ada_file_name) &
         """ not found (perhaps in exm or test subdirectory ?)");
      Failure;
  end Run_HAC_VM;

  function Remaining_Arguments (arg_pos : Positive) return String is
  (if arg_pos <= Ada.Command_Line.Argument_Count then
     Ada.Command_Line.Argument (arg_pos) & Remaining_Arguments (arg_pos + 1)
   else
     "");

  procedure Post_Build_amd64_windows_console_fasm
    (BD : HAC_Sys.Builder.Build_Data)
  is
    use HAT;
    use Ada.Directories;
    main_base_name : constant String := Base_Name (To_String (main_Ada_file_name));
  begin
    Shell_Execute
      ("fasm " &
       BD.target.Assembler_File_Name &
       ' ' &
       main_base_name &
       ".exe");
  end Post_Build_amd64_windows_console_fasm;

  procedure Run_amd64_windows_console_fasm (arg_pos : Positive)
  is
    use HAT;
    use Ada.Directories;
    main_base_name : constant String := Base_Name (To_String (main_Ada_file_name));
  begin
    if Get_Env ("OS") = "Windows_NT" then
      Shell_Execute (main_base_name & ' ' & Remaining_Arguments (arg_pos));
    else
      Put_Line ("No run: build target (AMD64/Windows) is different than this system");
    end if;
  end Run_amd64_windows_console_fasm;

  procedure Post_Build (BD : in out HAC_Sys.Builder.Build_Data) is
  begin
    case target_choice is
      when hac_vm                     => null;
      when amd64_windows_console_fasm => Post_Build_amd64_windows_console_fasm (BD);
    end case;
  end Post_Build;

  procedure Run (BD : in out HAC_Sys.Builder.Build_Data; arg_pos : Positive) is
    use HAT;
  begin
    if verbosity >= 2 then
      if BD.CD.Is_HAC_VM then
        Put_Line (HAC_margin_2 & "Starting p-code VM interpreter...");
      else
        Put_Line (HAC_margin_2 & "Running native (if target = native)");
      end if;
    end if;
    if target_choice /= hac_vm then
      Put_Line ("*** Caution *** Native code generation is experimental and incomplete !");
    end if;
    case target_choice is
      when hac_vm                     => Run_HAC_VM (BD, arg_pos);
      when amd64_windows_console_fasm => Run_amd64_windows_console_fasm (arg_pos);
    end case;
  end Run;

end HAC_Pkg;
