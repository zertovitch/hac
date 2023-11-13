--  HAC: command-line build and execution tool for HAC (HAC Ada Compiler)
--  Usage, license etc. : see `Help` below and the HAC_Sys package (hac_sys.ads).
--  For a small version, see HAC_Mini (hac_mini.adb).
--

with HAC_Pkg;

with HAC_Sys.Builder,
     HAC_Sys.Co_Defs,
     HAC_Sys.Defs,
     HAC_Sys.PCode.Interpreter.In_Defs;

with HAT;

with Ada.Calendar,
     Ada.Command_Line,
     Ada.Text_IO;

procedure HAC is

  cmp_dump_file_name : HAT.VString;
  asm_dump : Boolean := False;
  remarks : HAC_Sys.Defs.Remark_Set := HAC_Sys.Defs.default_remarks;

  use HAC_Pkg;

  procedure Compile_and_interpret_file (Ada_file_name : String; arg_pos : Positive) is
    use Ada.Calendar, Ada.Text_IO;

    source_stream   : HAC_Sys.Co_Defs.Source_Stream_Access;
    t1, t2          : Ada.Calendar.Time;
    BD              : HAC_Sys.Builder.Build_Data;
    shebang_offset  : Natural := 0;

    trace : constant HAC_Sys.Co_Defs.Compilation_Trace_Parameters :=
      (pipe         => null,
       progress     => Compilation_Feedback'Access,
       detail_level => verbosity);

    cat : aliased Path_Management.File_Catalogue;

  begin
    main_Ada_file_name := HAT.To_VString (Ada_file_name);
    if verbosity > 1 then
      New_Line;
      Put_Line (HAC_margin_1 & "HAC is free and open-source. Type ""hac"" for license.");
    end if;
    cat.Source_Open (Ada_file_name, source_stream);
    --  HAC_Sys.Builder.Skip_Shebang (f, shebang_offset);
    --  !! TBD restore that as a method of Abstract_File_Catalogue
    BD.Set_Diagnostic_Parameters (asm_dump, HAT.To_String (cmp_dump_file_name));
    BD.Set_Remark_Set (remarks);
    BD.Set_Main_Source_Stream (source_stream, Ada_file_name, shebang_offset);
    BD.Set_Message_Feedbacks (trace);
    BD.Set_Target (target);
    BD.LD.Set_Source_Access (cat'Unchecked_Access);
    t1 := Clock;
    BD.Build_Main;
    t2 := Clock;
    cat.Close (Ada_file_name);
    if verbosity >= 2 then
      Put_Line (
        HAC_margin_2 & "Build finished in" &
        Duration'Image (t2 - t1) &
        " seconds." &
        Integer'Image (BD.Total_Compiled_Lines) & " lines compiled in total."
      );
    end if;
    --
    if not BD.Build_Successful then
      PLCE ("Errors found, build failed.");
      HAC_Pkg.Failure;
      return;
    end if;
    if verbosity >= 2 then
      Put_Line (HAC_margin_2 & "Target . : " & BD.CD.target.Name);
      Put_Line (HAC_margin_2 & "CPU  . . : " & BD.CD.target.CPU);
      Put_Line (HAC_margin_2 & "OS . . . : " & BD.CD.target.OS);
      --
      if BD.CD.Is_HAC_VM then
        Put_Line
          (HAC_margin_2 & "Object code size:" &
             Natural'Image (BD.Object_Code_Size) &
             " of" &
             Natural'Image (HAC_Sys.Builder.Maximum_Object_Code_Size) &
             " Virtual Machine instructions.");
        if BD.Folded_Instructions + BD.Specialized_Instructions > 0 then
          Put_Line (HAC_margin_2 & "Code optimization:");
          Put_Line
            (HAC_margin_2 & "  " & Natural'Image (BD.Folded_Instructions) &
               " instructions folded");
          Put_Line
            (HAC_margin_2 & "  " & Natural'Image (BD.Specialized_Instructions) &
               " instructions specialized");
        end if;
      end if;
    end if;
    Post_Build (BD);
    if compile_only then
      null;
    elsif BD.CD.Is_Executable then
      Run (BD, arg_pos);
    else
      PLCE ("Can only execute a parameterless procedure");
    end if;
  exception
    when Name_Error =>
      PLCE
        (HAC_margin_3 &
         "Error: file """ & Ada_file_name &
         """ not found (perhaps in exm or test subdirectory ?)");
      HAC_Pkg.Failure;
  end Compile_and_interpret_file;

  hac_ing    : Boolean  := False;
  quit       : Boolean  := False;
  help_level : Positive := 1;

  procedure Argument_Error (msg : String) is
  begin
    PLCE (msg);
    NLCE;
    quit := True;
    delay 1.0;
  end Argument_Error;

  procedure Process_Argument (arg : String; arg_pos : Positive) is
    opt : constant String := arg (arg'First + 1 .. arg'Last);
    unknown_remark : Boolean;
    use HAT;
  begin
    if arg (arg'First) = '-' then
      if opt'Length = 0 then
        Argument_Error ("Missing option code after '-'");
        return;
      end if;
      case opt (opt'First) is
        when 'a' =>
          asm_dump := True;
        when 'c' =>
          compile_only := True;
        when 'd' =>
          cmp_dump_file_name := To_VString (compiler_dump_name);
        when 'h' =>
          if opt'Length > 1 and then opt (opt'First + 1) = '2' then
            help_level := 2;
          end if;
          quit := True;
        when 't' =>
          if opt'Length = 1 then
            Argument_Error ("Missing target");
          else
            declare
              new_target_name : constant String := opt (opt'First + 1 .. opt'Last);
            begin
              Set_Target (new_target_name);
            exception
              when Constraint_Error =>
                Argument_Error ("Unknown target " & new_target_name);
            end;
          end if;
        when 'r' =>
          if opt'Length = 1 then
            Argument_Error ("Missing remark switch");
          else
            for letter in opt'First + 1 .. opt'Last loop
              unknown_remark := True;
              for r in HAC_Sys.Defs.Compile_Remark loop
                if HAC_Sys.Defs.remark_letter (r) = opt (letter) then
                  remarks (r) := True;
                  unknown_remark := False;
                elsif To_Upper (HAC_Sys.Defs.remark_letter (r)) = opt (letter) then
                  remarks (r) := False;
                  unknown_remark := False;
                end if;
              end loop;
              if unknown_remark then
                Argument_Error ("Unknown remark switch '" & opt (letter) & ''');
                exit;
              end if;
            end loop;
          end if;
        when 'I' =>
          if command_line_source_path /= "" then
            command_line_source_path := command_line_source_path & ';';
          end if;
          command_line_source_path :=
            command_line_source_path & To_VString (opt (opt'First + 1 .. opt'Last));
        when 'v' =>
          verbosity := 1;
          if opt'Length > 1 and then opt (opt'First + 1) in '0' .. '9' then
            verbosity := Character'Pos (opt (opt'First + 1)) - Character'Pos ('0');
          end if;
        when others =>
          Argument_Error ("Unknown option: """ & arg & '"');
      end case;
    else
      Compile_and_interpret_file (arg, arg_pos);
      hac_ing := True;
      quit := True;  --  The other arguments are for the HAC program.
    end if;
  end Process_Argument;

  use Ada.Command_Line;

begin
  for i in 1 .. Argument_Count loop
    Process_Argument (Argument (i), i);
    exit when quit;
  end loop;
  if not hac_ing then
    Help (help_level);
    if verbosity > 1 then
      Ada.Text_IO.Put_Line ("Size of a HAC VM memory unit:" &
        Integer'Image (HAC_Sys.PCode.Interpreter.In_Defs.Data_Type'Size / 8) &
        " bytes"
      );
    end if;
  end if;
end HAC;
