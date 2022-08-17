with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Defs,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Modularity,
     HAC_Sys.Parser.Packages,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

with HAT;

with Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO;

package body HAC_Sys.Compiler is

  use Defs;

  procedure Set_Message_Feedbacks (
    CD           : in out Compiler_Data;
    trace_params : in     Compilation_Trace_Parameters
  )
  is
  begin
    CD.trace := trace_params;
  end Set_Message_Feedbacks;

  procedure Init (CUD : out Current_Unit_Data) is
  begin
    CUD.c := ' ';
    CUD.CC := 0;
    CUD.LL := 0;
    CUD.level_0_def.Clear;
  end Init;

  procedure Init (CD : out Compiler_Data) is
  begin
    CD.Arrays_Count          := 0;
    CD.Blocks_Count          := 0;
    CD.Float_Constants_Count := 0;
    CD.Packages_Count        := 0;
    --  Identifiers
    CD.Id_Count := 0;
    --  Strings literals
    CD.Strings_Table_Top := CD.Strings_Constants_Table'First;
    --  Tasks, Entries
    CD.Tasks_Definitions_Count := 0;
    CD.Entries_Count := 0;
    --  Location Counter (in output code)
    CD.LC   := 0;
    CD.CMax := CDMax;
    --  Code optimization:
    CD.folded_instructions      := 0;
    CD.specialized_instructions := 0;
    --  Current block name for debugging of HAC programs.
    CD.Full_Block_Id := Universe;
    --
    CD.Main_Program_ID           := Empty_Alfa;
    CD.Main_Program_ID_with_case := Empty_Alfa;
    --
    --  Current unit data
    --
    Init (CD.CUD);
    --  Scanner data
    CD.Sy                := Dummy_Symbol;
    CD.syStart           := 1;
    CD.syEnd             := 1;
    CD.prev_sy_start     := 1;
    CD.prev_sy_end       := 1;
    CD.prev_sy_line      := 0;
    CD.error_count       := 0;
    CD.minor_error_count := 0;
    CD.errs              := error_free;
    CD.total_lines       := 0;
    Scanner.InSymbol (CD);
    --
    CD.Display (0) := 0;  --  Added 7-Dec-2009
    CD.pkg_prefix := HAT.Null_VString;
  end Init;

  --  Print_Tables is for debugging purposes.
  --
  procedure Print_Tables (CD : in Compiler_Data) is
    use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Fixed;
    package HIIO is new Integer_IO (HAC_Integer);
    use HIIO;
    --
    function Cut_name (n : String; l : Natural) return String is
      dots : constant String := "...";
    begin
      if n'Length > l then
        return dots & n (n'Last - (l - 1) + dots'Length .. n'Last);
      else
        return n;
      end if;
    end Cut_name;
    --
    procedure Show_Padded (n : String; t : Positive) is
      trunc : constant String := Cut_name (n, t);
    begin
      Put (CD.comp_dump, "  " & trunc & Integer'Max (0, t - trunc'Length) * ' ');
    end Show_Padded;
    use type Alfa;
    Alng : constant := 50;  --  Max characters displayed on this dump.
  begin
    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump,
       " Identifiers" & (Alng - 6) * ' ' &
       "Link  Object                        " &
       "TYP                    Ref  Norm Lvl  Adr Blck"
    );
    Put_Line (CD.comp_dump,
       (Alng + Entity_Kind'Width + Typen'Width + Boolean'Width + 34) * '-'
    );
    --  We list all definitions, starting
    --  from Main (last Id of the "zero block" / standard).
    --
    for I in 1 .. CD.Id_Count loop
      declare
        r : IdTabEntry renames CD.IdTab (I);
      begin
        Put (CD.comp_dump, I, 4);
        Show_Padded (A2S (r.name_with_case), Alng);
        Put (CD.comp_dump, r.link, 4);
        Show_Padded (Entity_Kind'Image (r.entity), Entity_Kind'Width);
        Show_Padded (Typen'Image (r.xtyp.TYP), Typen'Width);
        Put (CD.comp_dump, r.xtyp.Ref, 5);
        Show_Padded (Boolean'Image (r.normal), Boolean'Width);
        Put (CD.comp_dump, Integer (r.lev), 3);
        Put (CD.comp_dump, r.adr_or_sz, 5);
        if r.block_or_pkg_ref > 0 then
          Put (CD.comp_dump, r.block_or_pkg_ref, 5);
        else
          Put (CD.comp_dump, "     ");
        end if;
        Put (CD.comp_dump, "  " & Declaration_Kind'Image (r.decl_kind));
        New_Line (CD.comp_dump);
      end;
    end loop;

    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump, " Tasks       Block#");
    for I in 1 .. CD.Tasks_Definitions_Count loop
      Put (CD.comp_dump, I, 4);
      Put (CD.comp_dump, ' ');
      Put (CD.comp_dump, A2S (CD.IdTab (CD.Tasks_Definitions_Table (I)).name) & "  ");
      Put (CD.comp_dump, CD.IdTab (CD.Tasks_Definitions_Table (I)).block_or_pkg_ref);
      New_Line (CD.comp_dump);
    end loop;

    New_Line (CD.comp_dump);

    if CD.Entries_Count > 0 then
      Put (CD.comp_dump, " Entries ");
      New_Line (CD.comp_dump);
      for I in 1 .. CD.Entries_Count loop
        Put (CD.comp_dump, I, 4);
        Put (CD.comp_dump,
             ' ' & A2S (CD.IdTab (CD.Entries_Table (I)).name) & " in Task " &
             A2S (CD.IdTab (
               CD.Tasks_Definitions_Table (CD.IdTab (CD.Entries_Table (I)).adr_or_sz)
             ).name)
        );
        New_Line (CD.comp_dump);
      end loop;
      New_Line (CD.comp_dump);
    end if;

    Put_Line (CD.comp_dump, " Blocks" & Alng * ' ' & "  Last_ID FPar LPar PSze Vsze");
    for I in 1 .. CD.Blocks_Count loop
      declare
        r : BTabEntry renames CD.Blocks_Table (I);
      begin
        Put (CD.comp_dump, I, 4);
        Show_Padded (A2S (r.Id), Alng);
        Put (CD.comp_dump, r.Last_Id_Idx, 10);
        Put (CD.comp_dump, r.First_Param_Id_Idx, 5);
        Put (CD.comp_dump, r.Last_Param_Id_Idx, 5);
        Put (CD.comp_dump, r.PSize, 5);
        Put (CD.comp_dump, r.VSize, 5);
        New_Line (CD.comp_dump);
      end;
    end loop;
    New_Line (CD.comp_dump);

    if CD.Arrays_Count = 0 then
      Put_Line (CD.comp_dump, " Arrays: none");
    else
      Put_Line (CD.comp_dump,
        " Array   | Index: typ_________  Element: typ_______ref    Low___High   El. Size Ar. Size"
      );
      for i in 1 .. CD.Arrays_Count loop
        declare
          r : ATabEntry renames CD.Arrays_Table (i);
          package TIO is new Enumeration_IO (Typen);
          use TIO;
          typ_img : String (1 .. Typen'Width);
        begin
          Put (CD.comp_dump, i, 7);
          Put (typ_img, r.Index_xTyp.TYP);    --  Padded
          Put (CD.comp_dump, "  | " & typ_img);
          Put (typ_img, r.Element_xTyp.TYP);  --  Padded
          Put (CD.comp_dump, "  " & typ_img);
          Put (CD.comp_dump, r.Element_xTyp.Ref, 3);
          Put (CD.comp_dump, r.Index_xTyp.Discrete_First, 7);
          Put (CD.comp_dump, r.Index_xTyp.Discrete_Last,  7);
          Put (CD.comp_dump, r.Element_Size, 11);
          Put (CD.comp_dump, r.Array_Size,    9);
        end;
        New_Line (CD.comp_dump);
      end loop;
    end if;
    New_Line (CD.comp_dump);

    if CD.Packages_Count = 0 then
      Put_Line (CD.comp_dump, " Packages: none");
    else
      Put_Line
        (CD.comp_dump, " Package  | First decl. | Last public | Last private");
      for i in 1 .. CD.Packages_Count loop
        declare
          p : Package_Table_Entry renames CD.Packages_Table (i);
        begin
          Put (CD.comp_dump, i, 8);
          Put (CD.comp_dump, p.first_public_declaration, 15);
          Put (CD.comp_dump, p.last_public_declaration,  14);
          Put (CD.comp_dump, p.last_private_declaration, 15);
        end;
        New_Line (CD.comp_dump);
      end loop;
    end if;
    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump, " Library Level visible identifiers (unordered list):");
    for l0 of CD.CUD.level_0_def loop
      Put_Line (CD.comp_dump, "    " & A2S (CD.IdTab (l0).name));
    end loop;
    New_Line (CD.comp_dump);

    if CD.Main_Program_ID /= Empty_Alfa then
      Put_Line (CD.comp_dump, " Information about Main procedure:");
      Put_Line (CD.comp_dump, "   Name    : " & A2S (CD.Main_Program_ID_with_case));
      Put_Line (CD.comp_dump, "   Block # : " &
        Defs.Index'Image (CD.IdTab (CD.Main_Proc_Id_Index).block_or_pkg_ref));
    end if;

  end Print_Tables;

  ---------------------------------------------------------------------------

  procedure Progress_Message (CD : Co_Defs.Compiler_Data; msg : String) is
  begin
    if CD.trace.progress = null then
      Ada.Text_IO.Put_Line (msg);
    else
      CD.trace.progress (msg);
    end if;
  end Progress_Message;

  procedure Dump_Asm (CD : Co_Defs.Compiler_Data; file_name : String) is
    use Ada.Text_IO;
    asm_dump : File_Type;
  begin
    Create (asm_dump, Out_File, file_name);
    PCode.Dump
      (CD.ObjCode (CD.ObjCode'First .. CD.LC - 1),  --  Dump only compiled part.
       CD.Strings_Constants_Table,
       CD.Float_Constants_Table,
       asm_dump);
    Close (asm_dump);
  end Dump_Asm;

  procedure Compile_Main (
    CD                 : in out Co_Defs.Compiler_Data;
    LD                 : in out Librarian.Library_Data;
    main_name_hint     :        String;  --  This is used for circular unit dependency detection
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  )
  is
    use Ada.Text_IO, Parser.Helpers, PCode, Errors;
    use type HAC_Integer, Alfa;

    map_file : File_Type;

    full_main_Id : HAT.VString;
    main_block : Parser.Block_Data_Type;
    main_file_name : constant String := HAT.VStr_Pkg.To_String (CD.CUD.source_file_name);

  begin  --  Compile_Main
    if CD.trace.detail_level >= 1 then
      Progress_Message (CD, "Compiling main: " & main_file_name);
    end if;

    Init (CD);

    CD.listing_requested := listing_file_name /= "";
    if CD.listing_requested then
      Create (CD.listing, Name => listing_file_name);
      Put_Line (CD.listing, Header);
    end if;
    CD.comp_dump_requested := cmp_dump_file_name /= "";
    if CD.comp_dump_requested then
      Create (CD.comp_dump, Name => cmp_dump_file_name);
      Put_Line (CD.comp_dump, "Compiler: check for main's context clause");
    end if;

    Librarian.Apply_WITH_USE_Standard (CD, LD);  --  The invisible "with Standard; use Standard;"
    Parser.Modularity.Context_Clause (CD, LD);   --  Parse the "with"'s and "use"'s, compile units.

    if CD.Sy /= PROCEDURE_Symbol then
      Error (CD, err_missing_a_procedure_declaration, " (main)", severity => major);  --  PROCEDURE Name is
    end if;
    Scanner.InSymbol (CD);
    loop
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing, severity => major);
      end if;
      full_main_Id := full_main_Id & CD.Id_with_case;
      Scanner.InSymbol (CD);
      exit when CD.Sy /= Period;
      --  Here we have a Parent.Child naming.
      Scanner.InSymbol (CD);
      --  !! TBD: do the implicit "with Parent;" here.
      full_main_Id := full_main_Id & '.';
    end loop;
    CD.Main_Program_ID_with_case := full_main_Id;
    CD.Main_Program_ID           := HAT.To_Upper (full_main_Id);
    if CD.Main_Program_ID /= main_name_hint then
      Error (CD, err_library_error,
        ": unit name """ & main_name_hint & """ expected in this file, found: """ &
        A2S (CD.Main_Program_ID) & '"',
        major
      );
    end if;
    if CD.Sy /= IS_Symbol then
      --  procedure Name IS
      Error (CD, err_syntax_error, ": main procedure should be parameterless", major);
    end if;

    if CD.comp_dump_requested then
      Put_Line (CD.comp_dump, "Compiler: main procedure is " & A2S (CD.Main_Program_ID));
    end if;

    Librarian.Enter_Library_Level_Def (CD, A2S (CD.Main_Program_ID_with_case), Prozedure, NOTYP, 0);
    CD.Main_Proc_Id_Index := CD.Id_Count;
    CD.Tasks_Definitions_Table (0) := CD.Id_Count;  --  Task Table Entry for main task.

    CD.Blocks_Table (0) :=  --  Block Table Entry for stuff before Main (probably useless)
     (Id                 => S2A ("--  Definitions before Main"),
      Last_Id_Idx        => CD.Main_Proc_Id_Index,
      First_Param_Id_Idx => 1,
      Last_Param_Id_Idx  => 0,
      PSize              => 0,
      VSize              => 0,
      SrcFrom            => CD.CUD.line_count,
      SrcTo              => CD.CUD.line_count);

    main_block.level                         := 1;
    main_block.block_id_index                := CD.Id_Count;
    main_block.entity                        := Prozedure;
    main_block.is_main                       := True;
    main_block.previous_declaration_id_index := No_Id;
    --  Start Compiling of Main
    Parser.Block (
      CD, Block_Begin_Symbol + Statement_Begin_Symbol,
      False, main_block,
      CD.Main_Program_ID,
      CD.Main_Program_ID_with_case
    );
    CD.total_lines := CD.total_lines + CD.CUD.line_count;  --  Add line count of main program.
    --  Main procedure is parsed. Block did not emit a final "exit",
    --  then we can opt for halting the machine instead.
    PCode_Emit.Emit (CD, k_Halt_Interpreter);

    if CD.Sy /= Semicolon then
      if CD.comp_dump_requested then
        Put_Line (CD.comp_dump, "Compile terminated BEFORE FILE END");
      end if;
      if CD.listing_requested then
        Put_Line (CD.listing, "Compile terminated BEFORE FILE END");
      end if;
    end if;

    if CD.Blocks_Table (1).VSize > StMax - (STKINCR * CD.Tasks_Definitions_Count) then
      Error (CD, err_stack_size, "");
    end if;
    CD.Blocks_Table (1).SrcTo := CD.CUD.line_count;

    if CD.listing_requested then
      Close (CD.listing);
    end if;

    if CD.errs /= error_free then
      Compilation_Errors_Summary (CD);
    end if;

    if var_map_file_name /= "" then
      Create (map_file, Out_File, var_map_file_name);
      Put_Line (map_file, "  -* Symbol Table *-");
      New_Line (map_file);
      Put_Line (map_file, "  LOC  Name       scope");
      Put_Line (map_file, "------------------------");
      New_Line (map_file);
      for Blk of CD.IdTab (CD.Blocks_Table (0).Last_Id_Idx + 1 .. CD.Id_Count) loop
        if Blk.entity = Variable then
          if Blk.xtyp.TYP /= NOTYP then
            Ada.Integer_Text_IO.Put (map_file, Blk.adr_or_sz, 4);
            Put (map_file, A2S (Blk.name) & "   ");
          end if;
          if Blk.lev = 1 then
            Put (map_file, " Global(");
          else
            Put (map_file, " Local (");
          end if;
          Put (map_file, Nesting_level'Image (Blk.lev));
          Put (map_file, ')');
          New_Line (map_file);
        end if;
      end loop;
      New_Line (map_file);
      Close (map_file);
    end if;

    if CD.trace.detail_level >= 2 then
      Progress_Message (CD, "Compilation of " & main_file_name & " (main) completed");
    end if;

  exception
    when End_Error =>
      Error (CD, err_unexpected_end_of_text);
  end Compile_Main;

  --
  --  !! Massively "W.I.P." state here !!
  --

  procedure Compile_Unit (
    CD                     : in out Co_Defs.Compiler_Data;
    LD                     : in out Librarian.Library_Data;
    upper_name             :        String;
    file_name              :        String;
    as_specification       :        Boolean;
    specification_id_index :        Natural;
    new_id_index           :    out Natural;
    unit_context           : in out Co_Defs.Id_Maps.Map;  --  in : empty for spec, spec's context for body
                                                          --  out: spec's context or body's full context.
    kind                   :    out Librarian.Unit_Kind;  --  The unit kind is discovered during parsing.
    needs_body             :    out Boolean
  )
  is
    use Ada.Strings.Fixed, Ada.Text_IO, Librarian, Errors, Parser.Helpers, PCode;
    --  Save state of unit currently parsed (within a WITH clause).
    mem : constant Current_Unit_Data := CD.CUD;
    Unit_Id_with_case : Alfa;
    unit_block : Parser.Block_Data_Type;
    indent : Natural := 0;
    src_stream : Co_Defs.Source_Stream_Access;
    function Spec_or_Body return String is
      (" (" & (if as_specification then "specification)" else "body)"));
  begin
    CD.recursion := CD.recursion + 1;
    if CD.trace.detail_level >= 1 then
      if CD.trace.detail_level >= 2 then
        indent := CD.recursion;
      end if;
      Progress_Message (CD, indent * '.' & "Compiling: " & file_name & Spec_or_Body);
    end if;
    begin
      LD.open_source (file_name, src_stream);
    exception
      when Name_Error =>
        Error (CD, err_library_error, "file " & file_name & Spec_or_Body & " not found", major);
    end;
    --  HAT.PUT_LINE("Compiling unit " & upper_name);
    Set_Source_Stream (CD.CUD, src_stream, file_name, 0);
    --  Reset scanner data (line counter etc.) and
    --  library-level visible declarations (processed WITH of caller's compilation)
    Init (CD.CUD);
    --  If we are compiling the body of a unit, unit_context already contains, automatically:
    --    - the WITH and USE context clauses of the spec,
    --    - the package's declarations, incuding the private part.
    --  Basically the body is a continuation of the spec, possibly in another file.
    CD.CUD.level_0_def := unit_context;

    --
    --  We define Standard, or activate if this is not the first unit compiled.
    --
    Librarian.Apply_WITH_USE_Standard (CD, LD);  --  The invisible "with Standard; use Standard;"
    --  HAT.PUT_LINE("Unit " & upper_name & " sees and uses Standard");

    Scanner.InSymbol (CD);
    Parser.Modularity.Context_Clause (CD, LD);   --  Parse the "with"'s and "use"'s, compile units.
    case CD.Sy is
      when PACKAGE_Symbol =>
        Scanner.InSymbol (CD);
        if CD.Sy = BODY_Symbol then
          Scanner.InSymbol (CD);  --  Absorb the BODY symbol.
          kind := Package_Body;
          if as_specification then
            Error (CD, err_library_error, "specification expected in this file; found body", major);
          end if;
        else
          kind := Package_Declaration;
          if not as_specification then
            Error (CD, err_library_error, "body expected in this file; found specification", major);
          end if;
        end if;
      when FUNCTION_Symbol =>
        kind := Function_Unit;
        Scanner.InSymbol (CD);
      when PROCEDURE_Symbol =>
        kind := Procedure_Unit;
        Scanner.InSymbol (CD);
      when others =>
        kind := Package_Declaration;  --  Useless, but this removes an ObjectAda warning.
        Error (CD, err_syntax_error, ": `package`, `procedure` or `function` expected here", major);
    end case;
    if CD.Sy /= IDent then
      Error (CD, err_identifier_missing, severity => major);
    end if;
    if A2S (CD.Id) /= upper_name then
      Error (CD, err_library_error, "unit name """ & upper_name & """ expected in this file", major);
    end if;
    --
    --  Enter the identifier:
    --
    Unit_Id_with_case := CD.Id_with_case;
    case kind is
      when Procedure_Unit =>
        Librarian.Enter_Library_Level_Def (CD, A2S (Unit_Id_with_case), Prozedure, NOTYP, 0);
      when Function_Unit =>
        Librarian.Enter_Library_Level_Def (CD, A2S (Unit_Id_with_case), Funktion, NOTYP, 0);
        --  The type of the return value is adjusted by Block.Function_Result_Profile.
      when Package_Declaration =>
        Librarian.Enter_Library_Level_Def (CD, A2S (Unit_Id_with_case), Paquetage, NOTYP, 0);
      when Package_Body =>
        null;  --  Library-level package body doesn't need an entry in the identifier table.
    end case;
    new_id_index := CD.Id_Count;
    case kind is
      when Subprogram_Unit =>
        Scanner.InSymbol (CD);  --  Absorb the identifier symbol.
        --  Here the symbol should be: ";", "IS", "(", or "RETURN" for a parameterless function.
        --
        unit_block.level                         := 1;
        unit_block.block_id_index                := new_id_index;
        unit_block.entity                        := (if kind = Function_Unit then Funktion else Prozedure);
        unit_block.is_main                       := False;
        unit_block.previous_declaration_id_index := specification_id_index;
        Parser.Block (
          CD, Block_Begin_Symbol + Statement_Begin_Symbol,
          False,
          unit_block,
          CD.IdTab (CD.Id_Count).name,
          Unit_Id_with_case
        );
        case Split_Declaration_Kind (CD.IdTab (unit_block.block_id_index).decl_kind) is
          when complete =>
            if as_specification then
              Error (CD, err_library_error, "specification expected in this file; found body", major);
            end if;
            if kind = Function_Unit then
              PCode_Emit.Emit_1 (CD, k_Exit_Function, End_Function_without_Return);
            else
              PCode_Emit.Emit_1 (CD, k_Exit_Call, Normal_Procedure_Call);
            end if;
          when spec_unresolved =>
            if not as_specification then
              Error (CD, err_library_error, "body expected in this file; found specification", major);
            end if;
          when spec_resolved =>
            raise Program_Error with "Unexpected case: spec_resolved";
        end case;
        needs_body := as_specification;
      when Package_Declaration =>
        unit_block.level := 0;  --  Actually, not a block.
        CD.IdTab (new_id_index).decl_kind := spec_resolved;
        --  Why spec_resolved ? missing bodies for eventual suprograms
        --  in that package are checked anyway.
        Parser.Packages.Package_Declaration (CD, Empty_Symset, unit_block, needs_body);
      when Package_Body =>
        unit_block.level := 0;  --  Actually, not a block.
        Parser.Packages.Package_Body (CD, Empty_Symset, unit_block);
        needs_body := False;
    end case;
    LD.close_source (file_name);
    if CD.trace.detail_level >= 2 then
      Progress_Message (CD,
       indent * '.' &
       "Compilation of " & file_name & " completed");
    end if;
    --  Export library-level context, possibly needed later by a body:
    unit_context := CD.CUD.level_0_def;
    CD.total_lines := CD.total_lines + CD.CUD.line_count;
    --  Forget about the compilation just completed, back to the ongoing
    --  compilation that triggered a call to Compile_Unit via a WITH:
    CD.CUD := mem;
    CD.recursion := CD.recursion - 1;
  exception
    when others =>
      LD.close_source (file_name);
      raise;
  end Compile_Unit;

  function Unit_Compilation_Successful (CD : Compiler_Data) return Boolean is
  begin
    return CD.error_count = 0;
  end Unit_Compilation_Successful;

  function Unit_Object_Code_Size (CD : Compiler_Data) return Natural is
  begin
    return CD.LC;
  end Unit_Object_Code_Size;

end HAC_Sys.Compiler;
