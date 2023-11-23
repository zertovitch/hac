with HAC_Sys.Compiler,
     HAC_Sys.Errors,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Targets.HAC_Virtual_Machine;

with Ada.Characters.Handling,
     Ada.Exceptions,
     Ada.Integer_Text_IO,
     Ada.Text_IO,
     Ada.Unchecked_Deallocation;

package body HAC_Sys.Builder is

  overriding procedure Finalize (BD : in out Build_Data) is

    procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation
        (Co_Defs.Compiler_Data, Co_Defs.Compiler_Data_Access);

  begin
    Unchecked_Free (BD.CD);
  end Finalize;

  procedure Compile_Pending_Bodies_Single_Round
    (BD : in out Build_Data; num_pending : out Natural)
  is
    use HAT, Librarian, Targets;
    pending : Library_Unit_Vectors.Vector;
    previous_context : Co_Defs.Id_Maps.Map;
    needs_body_dummy : Boolean;
  begin
    for lu of BD.LD.library loop
      if lu.status in Spec_Done then
        pending.Append (lu);
      end if;
    end loop;
    --
    --  Here: the list of pending bodies is now established
    --  for this round. Of course the library may expand
    --  further due to dependencies, via Register_Unit,
    --  adding pending bodies for the next round.
    --
    num_pending := 0;
    if BD.CD.error_count > 0 then
      return;
    end if;
    for lu of pending loop
      declare
        upper_vname : constant VString := To_Upper (lu.full_name);
        upper_name : constant String := To_String (upper_vname);
        fn_spec : constant String := Find_Unit_File_Name (BD.LD, upper_name);
        fn_body : constant String := BD.LD.cat.Full_Body_Source_Name (fn_spec);
      begin
        case Spec_Done (lu.status) is
          when Body_Postponed =>

            previous_context :=
              BD.LD.library.Element (BD.LD.map.Element (upper_vname)).spec_context;
            BD.CD.remarks := BD.global_remarks;
            if BD.target /= null then
              BD.CD.target  := BD.target;
            end if;

            Compiler.Compile_Unit
              (CD                     => BD.CD.all,
               LD                     => BD.LD,
               upper_name             => upper_name,
               file_name              => fn_body,
               as_specification       => False,
               as_main_unit           => upper_name = Defs.A2S (BD.CD.Id),
               needs_opening_a_stream => True,
               first_compilation      => False,
               specification_id_index => lu.id_index,
               new_id_index           => lu.id_body_index,
               unit_context           => previous_context,
               kind                   => lu.kind,
               needs_body             => needs_body_dummy);

            exit when BD.CD.error_count > 0;

            num_pending := num_pending + 1;

          when Spec_Only =>

            if BD.LD.cat.Exists (fn_body) then
              Errors.Error
                (BD.CD.all,
                 Defs.err_library_error,
                 "library package declaration shall not have a body unless it " &
                   "requires a body (Ada RM 7.2 (4)); found the file: " & fn_body);
            end if;

        end case;

        lu.status := Done;
        Change_Unit_Details (BD.LD, lu);
      end;
    end loop;
  end Compile_Pending_Bodies_Single_Round;

  procedure Build_Main
    (BD                            : in out Build_Data;
     body_compilation_rounds_limit :        Rounds_Range := full_build)
  is
    use Co_Defs, Defs, HAT.VStr_Pkg, Librarian, Targets;
    use Ada.Exceptions, Ada.Text_IO;

    main_unit : Library_Unit :=
      (full_name     => BD.main_name_hint,
       kind          => Procedure_Unit,
       status        => In_Progress,  --  Temporary value.
       id_index      => No_Id,        --  Temporary value.
       id_body_index => No_Id,        --  Temporary value.
       spec_context  => Id_Maps.Empty_Map);

    procedure Finalize_Target is
    begin
      BD.CD.target.Finalize_Code_Emission
        (BD.CD.Strings_Constants_Table (1 .. BD.CD.Strings_Table_Top));
    end Finalize_Target;

    procedure Progress (s : String; min_level : Positive) is
    begin
      if BD.CD.trace.detail_level >= min_level then
        Compiler.Progress_Message (BD.CD.all, s);
      end if;
    end Progress;

    procedure Complete_Graph_Build is
      num_pending : Natural;
    begin
      if body_compilation_rounds_limit > 0 then
        Progress
          ("------  Compilation of possibly uncompiled unit bodies  ------", 2);
      end if;
      for round in 1 .. body_compilation_rounds_limit loop
        Compile_Pending_Bodies_Single_Round (BD, num_pending);
        --  Now, other bodies may have appeared that have
        --  not been yet compiled.
        if num_pending > 0 then
          Progress
            ("------  End of Round" & round'Image &
             ", compiled bodies:" & num_pending'Image & "  ------", 2);
        end if;
        exit when num_pending = 0;
      end loop;
    end Complete_Graph_Build;

    procedure Dump_Object_Map (var_map_file_name : String) is
      map_file : File_Type;
      use type HAC_Integer;
    begin
      Create (map_file, Out_File, var_map_file_name);
      Put_Line (map_file, "  -* Symbol Table *-");
      New_Line (map_file);
      Put_Line (map_file, "  LOC  Name       scope");
      Put_Line (map_file, "------------------------");
      New_Line (map_file);
      for Blk of BD.CD.IdTab (BD.CD.Blocks_Table (0).Last_Id_Idx + 1 .. BD.CD.Id_Count) loop
        if Blk.entity in Object_Kind then
          if Blk.xtyp.TYP /= NOTYP then
            Ada.Integer_Text_IO.Put (map_file, Integer (Blk.adr_or_sz), 4);
            Put (map_file, A2S (Blk.name) & "   ");
          end if;
          if Blk.lev = 1 then  --  TBD: check this, should be 0.
            Put (map_file, " Global(");
          else
            Put (map_file, " Local (");
          end if;
          Put (map_file, Blk.lev'Image);
          Put (map_file, ')');
          New_Line (map_file);
        end if;
      end loop;
      New_Line (map_file);
      Close (map_file);
    end Dump_Object_Map;

    procedure Restart_with_Spec is
      fn_body : constant String := HAT.To_String (BD.CD.CUD.source_file_name);
    begin
      if BD.LD.cat.Is_Open (fn_body) then
        BD.LD.cat.Close (fn_body);
      end if;
      Progress ("\---> Cannot start build with a package's body.", 1);
      Progress ("      We restart from a possible spec.", 1);

      BD.Build_Main_from_File
        (file_name =>
           (if fn_body'Length = 0 then
              --  The body hasn't a file name (like just typed in an editor bound to HAC).
              --  Let's try to invent a name for the spec.
              Librarian.GNAT_File_Naming (A2S (BD.CD.main_unit_ident_with_case)) & ".ads"
            else
              BD.LD.cat.Full_Spec_Source_Name (fn_body)),
            --
         body_compilation_rounds_limit =>
           (if body_compilation_rounds_limit = Rounds_Range'Last then
              Rounds_Range'Last
            else
              body_compilation_rounds_limit + 1));
    end Restart_with_Spec;

    procedure Build_Main_Inner is
      main_file_name : constant String := To_String (BD.CD.CUD.source_file_name);

      new_id_index : Natural;
      needs_body : Boolean;
      as_specification : Boolean;

    begin
      BD.LD.library.Clear;
      BD.LD.map.Clear;

      --  The main unit is from the beginning registered with the In_Progress
      --  status, so we can catch a possible circular dependency of the main
      --  unit on itself - directly or indirectly.
      --
      --     Examples:
      --                   with A; procedure A is begin null; end;
      --
      --                   with Y; procedure X is begin null; end;
      --                   with X; procedure Y is begin null; end;
      --
      Librarian.Register_Unit (BD.LD, main_unit);

      BD.CD.remarks := BD.global_remarks;
      if BD.target /= null then
        BD.CD.target := BD.target;
      end if;

      BD.CD.listing_requested := BD.listing_file_name /= "";
      if BD.CD.listing_requested then
        Create (BD.CD.listing, Name => To_String (BD.listing_file_name));
        Put_Line (BD.CD.listing, Defs.Header);
      end if;

      BD.CD.comp_dump_requested := BD.cmp_dump_file_name /= "";
      if BD.CD.comp_dump_requested then
        Create (BD.CD.comp_dump, Name => To_String (BD.cmp_dump_file_name));
        Put_Line (BD.CD.comp_dump,
          "Compiler: main unit file name is " & main_file_name);
      end if;

      Progress ("HAC Ada Compiler version " & version & ", " & reference, 1);
      Progress ("Compiling main: " & main_file_name, 1);

      begin
        Compiler.Init_for_new_Build (BD.CD.all);
      exception
        when End_Error =>
          --  Happens if the text stream is empty.
          Errors.Error (BD.CD.all, err_unexpected_end_of_text, severity => Errors.major);
      end;

      as_specification := main_file_name (main_file_name'Last) = 's';

      Compiler.Compile_Unit
        (CD                     => BD.CD.all,
         LD                     => BD.LD,
         upper_name             => To_String (BD.main_name_hint),
         file_name              => main_file_name,
         as_specification       => as_specification,
         as_main_unit           => True,
         needs_opening_a_stream => False,
         first_compilation      => True,
         specification_id_index => No_Id,
         new_id_index           => new_id_index,
         unit_context           => main_unit.spec_context,
         kind                   => main_unit.kind,
         needs_body             => needs_body);

      if as_specification then
        case main_unit.kind is
          when Subprogram_Unit =>
            main_unit.status := Body_Postponed;
          when Package_Declaration =>
            main_unit.status := (if needs_body then Body_Postponed else Spec_Only);
          when Package_Body =>
            null;  --  Not relevant (spec.)
        end case;
      else
        case main_unit.kind is
          when Procedure_Unit =>
            --  !!  The following should be performed by Statements_Part_Closing
            --      in Parser... But it doesn't happen for the main block.
            BD.CD.Blocks_Table (1).SrcTo := BD.CD.CUD.location.line;
          when Function_Unit =>
            null;
          when Package_Body =>
            null;
          when Package_Declaration =>
            null;  --  not relevant
        end case;
        main_unit.status := Done;
      end if;

      Progress ("Compilation of " & main_file_name & " (main) completed", 2);

      main_unit.id_index := new_id_index;
      Librarian.Change_Unit_Details (BD.LD, main_unit);
      --
      --  Here: compilation of Main unit is finished (with or without
      --  minor or medium errors).
      --
      Complete_Graph_Build;
      --
      --  Here: build of the whole unit graph is finished (with or without
      --  minor or medium errors).
      --
      Finalize_Target;
      --
      if BD.CD.LC > BD.CD.ObjCode'First
        and then BD.CD.target.all not in Targets.HAC_Virtual_Machine.Machine'Class
      then
        --  Some machine code was emitted for the HAC VM instead of the alternative target.
        Errors.Error
          (BD.CD.all,
           err_general_error,
           "Code generation for alternative target (non-HAC-VM) is incomplete");
      end if;
      --
      if BD.CD.error_count = 0 then
        Parser.Helpers.Check_Incomplete_Definitions (BD.CD.all, 0);
      end if;

      if BD.CD.diags /= no_diagnostic then
        Errors.Compilation_Diagnostics_Summary (BD.CD.all);
      end if;

      if BD.CD.comp_dump_requested then
        Compiler.Print_Tables (BD.CD.all);
        Close (BD.CD.comp_dump);
      end if;
      if BD.asm_dump then
        Compiler.Dump_HAC_VM_Asm (BD.CD.all, BD.CD.target.Assembler_File_Name);
      end if;
      if BD.CD.listing_requested then
        Close (BD.CD.listing);
      end if;
      if Length (BD.obj_map_file_name) > 0 then
        Dump_Object_Map (To_String (BD.obj_map_file_name));
      end if;
    exception
      when Errors.Compilation_of_package_body_before_spec =>
        Restart_with_Spec;
      when E : HAC_Sys.Librarian.Circular_Unit_Dependency =>
        Finalize_Target;  --  Needed even on incomplete compilation.
        Errors.Error
          (BD.CD.all,
           Defs.err_library_error,
           "circular unit dependency (""->"" means ""depends on""): " &
           To_String (BD.main_name_hint) & " -> " &
           Exception_Message (E));
    end Build_Main_Inner;

  begin
    Build_Main_Inner;
  exception
    when Errors.Compilation_abandoned =>
      --  Hit a severe error...
      Finalize_Target;  --  Needed even on incomplete compilation.
      Errors.Compilation_Diagnostics_Summary (BD.CD.all);
      if BD.CD.comp_dump_requested then
        Compiler.Print_Tables (BD.CD.all);
        Close (BD.CD.comp_dump);
      end if;
      if BD.asm_dump then
        Compiler.Dump_HAC_VM_Asm (BD.CD.all, BD.CD.target.Assembler_File_Name);
      end if;
  end Build_Main;

  procedure Build_Main_from_File
    (BD                            : in out Build_Data;
     file_name                     :        String;
     body_compilation_rounds_limit :        Rounds_Range := full_build)
  is
    source_stream : Co_Defs.Source_Stream_Access;
  begin
    if BD.LD.cat.Exists (file_name) then
      BD.LD.cat.Source_Open (file_name, source_stream);
    else
      Errors.Error
        (BD.CD.all, Defs.err_library_error,
         "file " & file_name & " not found", severity => Errors.major);
    end if;
    BD.Set_Main_Source_Stream (source_stream, file_name);
    BD.Build_Main (body_compilation_rounds_limit);
    BD.LD.cat.Close (file_name);
  end Build_Main_from_File;

  procedure Set_Diagnostic_Parameters
    (BD                 : in out Build_Data;
     asm_dump           :        Boolean := False;  --  Assembler output of compiled object code
     cmp_dump_file_name :        String  := "";     --  Compiler dump
     listing_file_name  :        String  := "";     --  Listing of source code with details
     obj_map_file_name  :        String  := "")     --  Output of objects (map)
  is
    use HAT;
  begin
    BD.asm_dump           := asm_dump;
    BD.cmp_dump_file_name := To_VString (cmp_dump_file_name);
    BD.listing_file_name  := To_VString (listing_file_name);
    BD.obj_map_file_name  := To_VString (obj_map_file_name);
  end Set_Diagnostic_Parameters;

  procedure Set_Remark_Set
    (BD  : in out Build_Data;
     set : in     Defs.Remark_Set) is
  begin
    BD.global_remarks := set;
  end Set_Remark_Set;

  --  Set current main source stream (file, editor data, zipped file,...)
  procedure Set_Main_Source_Stream
    (BD         : in out Build_Data;
     s          : access Ada.Streams.Root_Stream_Type'Class;
     file_name  : in     String;        --  Can be a virtual name (editor title, zip entry)
     start_line : in     Natural := 0)  --  We could have a shebang or other Ada sources before
  is
    main_name_guess : String := Ada.Characters.Handling.To_Upper (file_name);
    last_slash, last_dot : Natural := 0;
  begin
    Co_Defs.Set_Source_Stream (BD.CD.CUD, s, file_name, start_line);
    --  Guess unit name from file name (operation is the reverse of GNAT_Naming in Librarian).
    for i in main_name_guess'Range loop
      case main_name_guess (i) is
        when '.'       => last_dot := i;
        when '/' | '\' => last_slash := i;
        when '-'       => main_name_guess (i) := '.';  --  Child unit (GNAT naming convention)
        when others    => null;
      end case;
    end loop;
    if last_dot = 0  --  no dot at all
      or else last_dot < last_slash  --  dot only in a path
    then
      last_dot := main_name_guess'Last + 1;
    end if;
    BD.main_name_hint := HAT.To_VString (main_name_guess (last_slash + 1 .. last_dot - 1));
  end Set_Main_Source_Stream;

  procedure Set_Message_Feedbacks
    (BD           : in out Build_Data;
     trace_params : in     Co_Defs.Compilation_Trace_Parameters)
  is
  begin
    Compiler.Set_Message_Feedbacks (BD.CD.all, trace_params);
  end Set_Message_Feedbacks;

  procedure Set_Target
    (BD         : in out Build_Data;
     new_target :        Targets.Abstract_Machine_Reference)
  is
    use Targets;
  begin
    if new_target /= null then
      BD.target := new_target;
    end if;
  end Set_Target;

  function Build_Successful (BD : Build_Data) return Boolean is
  begin
    return Compiler.Unit_Compilation_Successful (BD.CD.all);
    --  NB: currently, only full builds are supported.
  end Build_Successful;

  function Total_Compiled_Lines (BD : Build_Data) return Natural is
  begin
    return BD.CD.total_lines;
  end Total_Compiled_Lines;

  function Object_Code_Size (BD : Build_Data) return Natural is
  begin
    return Compiler.Unit_Object_Code_Size (BD.CD.all);
    --  Whatever the build mode, the entire object code lands into BD.CD's object code.
  end Object_Code_Size;

  function Folded_Instructions (BD : Build_Data) return Natural is
  begin
    return BD.CD.folded_instructions;
  end Folded_Instructions;

  function Specialized_Instructions (BD : Build_Data) return Natural is
  begin
    return BD.CD.specialized_instructions;
  end Specialized_Instructions;

  function Maximum_Object_Code_Size return Natural is
  begin
    return Defs.CDMax;
  end Maximum_Object_Code_Size;

end HAC_Sys.Builder;
