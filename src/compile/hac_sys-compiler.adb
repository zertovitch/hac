with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Defs,
     HAC_Sys.Errors,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Modularity,
     HAC_Sys.Parser.Packages,
     HAC_Sys.PCode,
     HAC_Sys.Scanner;

with HAT;

with Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO;

package body HAC_Sys.Compiler is

  use Defs;

  procedure Set_Message_Feedbacks
    (CD           : in out Compiler_Data;
     trace_params : in     Compilation_Trace_Parameters)
  is
  begin
    CD.trace := trace_params;
  end Set_Message_Feedbacks;

  --  Initialize the compiler for a new unit.
  procedure Init (CUD : in out Current_Unit_Data) is
  begin
    CUD.buffer_length   := 0;
    CUD.buffer_position := 1;
    CUD.c := ' ';
    CUD.CC       := 0;
    CUD.LL       := 0;
    CUD.location := (0, 1, 1);
    CUD.level_0_def.Clear;
    CUD.use_hat_stack_top := 0;
    CUD.Use_HAT_Stack (CUD.use_hat_stack_top) := False;
  end Init;

  --  Initialize the compiler for an entire build.
  procedure Init_for_new_Build (CD : out Compiler_Data) is
  begin
    CD.Arrays_Count          := 0;
    CD.Blocks_Count          := 0;
    CD.Float_Constants_Count := 0;
    CD.loop_nesting_level    := 0;
    CD.Packages_Count        := 0;
    --  Identifiers
    CD.Id_Count := 0;
    CD.IdTab (CD.Id_Count).name := Empty_Alfa;
    --  Strings literals
    CD.Strings_Table_Top := Strings_Constants_Table_Type'First - 1;
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
    CD.main_unit_ident           := Empty_Alfa;
    CD.main_unit_ident_with_case := Empty_Alfa;
    CD.main_proc_id_index        := No_Id;
    --
    --  Current unit data
    --
    Init (CD.CUD);
    --  Scanner data
    CD.Sy                := Dummy_Symbol;
    CD.prev_sy_loc       := (0, 1, 1);
    CD.error_count       := 0;
    CD.minor_error_count := 0;
    CD.diags             := no_diagnostic;
    CD.total_lines       := 0;
    Scanner.In_Symbol (CD);
    --
    CD.Display (0) := 0;  --  Added 7-Dec-2009
    CD.pkg_prefix := HAT.Null_VString;
    --
    CD.target.Initialize_Code_Emission;
    --
    --  Block Table Entry 0 is not a real block but serves only for
    --  its index in the identifier table, which lists global,
    --  level 0 stuff, outside any subprogram including Main.
    --  This entry is accessed by Locate_Identifier_Internal (implicitly),
    --  Dump_Object_Map, Enter_Library_Level_Def (explicitly).
    CD.Blocks_Table (0) :=
     (Id                 => S2A ("--  Definitions at level 0"),
      Last_Id_Idx        => 0,  --  Updated by Enter_Library_Level_Def.
      First_Param_Id_Idx => 1,
      Last_Param_Id_Idx  => 0,
      PSize              => 0,
      VSize              => 0,
      SrcFrom            => 1,
      SrcTo              => 1);
  end Init_for_new_Build;

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
               CD.Tasks_Definitions_Table (Integer (CD.IdTab (CD.Entries_Table (I)).adr_or_sz))
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
      Put_Line
        (CD.comp_dump,
         " Array   | Index: typ_________ " &
         " Element: typ_______ref   " &
         " Low___High   El. Size Ar. Size Dims");
      --
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
          Put (CD.comp_dump, r.dimensions,    5);
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

    if CD.main_unit_ident /= Empty_Alfa then
      Put_Line (CD.comp_dump, " Information about Main procedure:");
      Put_Line (CD.comp_dump, "   Name    : " & A2S (CD.main_unit_ident_with_case));
      Put_Line (CD.comp_dump, "   Block # : " &
        Defs.Index'Image (CD.IdTab (CD.main_proc_id_index).block_or_pkg_ref));
    end if;

    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump, "String table. Length:" & CD.Strings_Table_Top'Image);
    Put_Line (CD.comp_dump, "----");
    for i in Strings_Constants_Table_Type'First .. CD.Strings_Table_Top loop
      Put (CD.comp_dump, CD.Strings_Constants_Table (i));
      if i mod 70 = 0 or else i = CD.Strings_Table_Top then
        New_Line (CD.comp_dump);
      end if;
    end loop;
    Put_Line (CD.comp_dump, "----");
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

  procedure Dump_HAC_VM_Asm (CD : Co_Defs.Compiler_Data; file_name : String) is
    use Ada.Text_IO;
    asm_dump : File_Type;
  begin
    if CD.Is_HAC_VM then
      Create (asm_dump, Out_File, file_name);
      PCode.Dump
        (CD.ObjCode (CD.ObjCode'First .. CD.LC - 1),  --  Dump only compiled part.
         CD.Strings_Constants_Table,
         CD.Float_Constants_Table,
         asm_dump);
      Close (asm_dump);
    end if;
  end Dump_HAC_VM_Asm;

  procedure Compile_Unit
    (CD                     : in out Co_Defs.Compiler_Data;
     LD                     : in out Librarian.Library_Data;
     upper_name             :        String;
     file_name              :        String;
     as_specification       :        Boolean;
     as_main_unit           :        Boolean;
     needs_opening_a_stream :        Boolean;
     first_compilation      :        Boolean;  --  First compilation of whole build
     specification_id_index :        Natural;
     new_id_index           :    out Natural;
     unit_context           : in out Co_Defs.Id_Maps.Map;  --  in : empty for spec, spec's context for body
                                                           --  out: spec's context or body's full context.
     kind                   :    out Librarian.Unit_Kind;  --  The unit kind is discovered during parsing.
     needs_body             :    out Boolean)
  is
    use Ada.Strings.Fixed, Ada.Text_IO, Errors, Librarian, Parser.Helpers, PCode;
    --
    --  Save state of unit currently being parsed (within a WITH clause).
    --  That compilation is frozen until the point where `mem` is copied
    --  back to CD.CUD.
    mem : constant Current_Unit_Data := CD.CUD;
    --
    Unit_Id_with_case : Alfa;
    unit_block : Parser.Block_Data_Type;
    indent : Natural := 0;
    src_stream : Co_Defs.Source_Stream_Access;
    function Spec_or_Body return String is
      (" (" & (if as_specification then "specification)" else "body)"));
    --
    procedure Reactivate_USE_HAT is
      --  Detect a directly visible item of the HAT package.
      --  It that case, it proves that a "USE HAT" was in the context
      --  clause of the specification.
      some_stuff_in_HAT_str : constant String := "VSTRING";
      some_stuff_in_HAT     : constant Alfa := S2A (some_stuff_in_HAT_str);
      stuff_index : Integer;
    begin
      if unit_context.Contains (some_stuff_in_HAT) then
        stuff_index := unit_context (some_stuff_in_HAT);
        if CD.IdTab (stuff_index).entity = alias then
          --  Item named VSTRING from a USE clause was detected.
          --  Get the real item behind the alias (VSTRING -> ?.VSTRING):
          stuff_index := Integer (CD.IdTab (stuff_index).adr_or_sz);
          if A2S (CD.IdTab (stuff_index).name) = HAT_Name & '.' & some_stuff_in_HAT_str then
            --  Now we are sure the item stems from the HAT package.
            --  Normally, the full name is "HAT.VSTRING", unless HAT_Name has been customized.
            CD.CUD.Use_HAT_Stack (CD.CUD.use_hat_stack_top) := True;
          end if;
        end if;
      end if;
    end Reactivate_USE_HAT;
    --
    function Indent_String (starting : Boolean) return String is
      (case indent is
         when 0 => "",
         when 1 => "| ",
         when others =>
           (indent - 1) * ' ' &
           (if starting then '\' else '/') & ' ');

    full_file_name : constant String := LD.cat.Full_Source_Name (file_name);

  begin
    CD.recursion := CD.recursion + 1;
    if CD.trace.detail_level >= 1 then
      if CD.trace.detail_level >= 2 then
        indent := CD.recursion;
      end if;
      Progress_Message
        (CD,
         Indent_String (True) & "Compiling " &
         file_name & Spec_or_Body);
    end if;
    if needs_opening_a_stream then
      begin
        LD.cat.Source_Open (full_file_name, src_stream);
        Set_Source_Stream (CD.CUD, src_stream, full_file_name, 0);
      exception
        when Name_Error =>
          Error
            (CD, err_library_error,
             "file " & file_name & Spec_or_Body & " not found", severity => major);
      end;
    end if;
    if not first_compilation then
      --  Reset scanner data (line counter etc.) and
      --  library-level visible declarations (processed WITH of caller's compilation)
      Init (CD.CUD);
      --  If we are compiling the body of a unit, unit_context already contains, automatically:
      --    - the WITH and USE context clauses of the spec,
      --    - the package's declarations, incuding the private part.
      --  Basically the body is a continuation of the spec, possibly in another file.
      CD.CUD.level_0_def := unit_context;
      Reactivate_USE_HAT;
      Scanner.In_Symbol (CD);
    end if;
    --
    --  We define Standard, or activate if this is not the first unit compiled.
    --
    Librarian.Apply_WITH_USE_Standard (CD, LD);  --  The invisible "with Standard; use Standard;"
    --  HAT.PUT_LINE("Unit " & upper_name & " sees and uses Standard");

    Parser.Modularity.Context_Clause (CD, LD);  --  Parse the "with"'s and "use"'s, compile units.
    case CD.Sy is
      when PACKAGE_Symbol =>
        Scanner.In_Symbol (CD);
        if CD.Sy = BODY_Symbol then
          Scanner.In_Symbol (CD);  --  Absorb the BODY symbol.
          kind := Package_Body;
          if as_specification then
            Error
              (CD, err_library_error,
               "specification expected in this file; found body", severity => major);
          end if;
        else
          kind := Package_Declaration;
          if not as_specification then
            Error
              (CD, err_library_error,
               "body expected in this file; found specification", severity => major);
          end if;
        end if;
      when FUNCTION_Symbol =>
        kind := Function_Unit;
        Scanner.In_Symbol (CD);
      when PROCEDURE_Symbol =>
        kind := Procedure_Unit;
        Scanner.In_Symbol (CD);
      when others =>
        kind := Package_Declaration;  --  Useless, but this removes an ObjectAda warning.
        Error
          (CD,
           err_general_error,
           "`package`, `procedure` or `function` expected here", severity => major);
    end case;
    if CD.Sy /= IDent then
      Error (CD, err_identifier_missing, severity => major);
    end if;
    if as_main_unit then
      CD.main_unit_ident           := CD.Id;
      CD.main_unit_ident_with_case := CD.Id_with_case;
    end if;
    if A2S (CD.Id) /= upper_name then
      Error (CD, err_wrong_unit_name, upper_name, A2S (CD.Id), major);
    end if;
    if first_compilation and then kind = Package_Body then
      raise Compilation_of_package_body_before_spec;
    end if;
    --
    --  Enter the identifier:
    --
    Unit_Id_with_case := CD.Id_with_case;
    case kind is
      when Procedure_Unit =>
        Librarian.Enter_Library_Level_Def (CD, A2S (Unit_Id_with_case), prozedure, NOTYP, 0);
      when Function_Unit =>
        Librarian.Enter_Library_Level_Def (CD, A2S (Unit_Id_with_case), funktion, NOTYP, 0);
        --  ^ The type of the return value is adjusted by Block.Function_Result_Profile.
      when Package_Declaration =>
        Librarian.Enter_Library_Level_Def (CD, A2S (Unit_Id_with_case), paquetage, NOTYP, 0);
      when Package_Body =>
        Librarian.Enter_Library_Level_Def (CD, A2S (Unit_Id_with_case), paquetage_body, NOTYP, 0);
        --  ^ The identifier is used only by the Semantics target.
    end case;
    new_id_index := CD.Id_Count;
    if specification_id_index /= No_Id then
      CD.target.Mark_Spec_Body_Cross_References
        (spec_id => specification_id_index,
         body_id => new_id_index);
    end if;

    case kind is
      when Subprogram_Unit =>
        --  Absorb the identifier symbol:
        Scanner.In_Symbol (CD);
        --
        --  At this point, the current symbol should be: ";", "IS", "(",
        --  or, for a parameterless function, "RETURN".
        --
        unit_block.context.level                 := 1;
        unit_block.block_id_index                := new_id_index;
        unit_block.entity                        := (if kind = Function_Unit then funktion else prozedure);
        unit_block.is_main                       := as_main_unit;
        unit_block.previous_declaration_id_index := specification_id_index;
        Parser.Block
          (CD, Block_Begin_Symbol + Statement_Begin_Symbol,
           False,
           unit_block,
           CD.IdTab (CD.Id_Count).name,
           Unit_Id_with_case);
        if as_main_unit then
          if kind = Procedure_Unit
            and then Number_of_Parameters (CD, unit_block.block_id_index) = 0
          then
            --  This unit can be executed.
            CD.main_proc_id_index          := unit_block.block_id_index;
            CD.Tasks_Definitions_Table (0) := unit_block.block_id_index;  --  Task Table Entry for main task.
          else
            CD.main_proc_id_index := No_Id;
          end if;
        end if;
        case Split_Declaration_Kind (CD.IdTab (unit_block.block_id_index).decl_kind) is
          when complete =>
            if as_specification then
              Error
                (CD, err_library_error,
                 "specification expected in this file; found body", severity => major);
            end if;
            if kind = Function_Unit then
              --  When this part of the machine code is reached, it means
              --  that the end of a function was reached without
              --  a "RETURN" statement. This will raise Program_Error.
              PCode_Emit.Emit_1 (CD, k_Return_Function, End_Function_without_Return);
            elsif as_main_unit then
              CD.target.Emit_Halt;
            else
              PCode_Emit.Emit_1 (CD, k_Return_Call, Normal_Procedure_Call);
            end if;
          when spec_unresolved =>
            if not as_specification then
              Error
                (CD, err_library_error,
                 "body expected in this file; found specification", severity => major);
            end if;
          when spec_resolved =>
            raise Program_Error with "Unexpected case: spec_resolved";
        end case;
        needs_body := as_specification;

      when Package_Declaration =>
        unit_block.context.level := 0;  --  Actually, not a block.
        CD.IdTab (new_id_index).decl_kind := spec_resolved;
        --  Why spec_resolved ? missing bodies for possible suprograms
        --  in that package are checked anyway.
        Parser.Packages.Package_Declaration (CD, empty_symset, unit_block, needs_body);

      when Package_Body =>
        unit_block.context.level := 0;  --  Actually, not a block.
        Parser.Packages.Package_Body (CD, empty_symset, unit_block);
        needs_body := False;
    end case;

    if needs_opening_a_stream then
      LD.cat.Close (full_file_name);
    end if;
    if CD.trace.detail_level >= 2 then
      Progress_Message
        (CD,
         Indent_String (False) &
         "          " & file_name & ": done.");
    end if;
    --  Export library-level context, possibly needed later by a body:
    unit_context := CD.CUD.level_0_def;
    CD.total_lines := CD.total_lines + CD.CUD.location.line;
    --  Forget about the compilation just completed, and go back to the
    --  ongoing compilation that triggered a call to Compile_Unit via a WITH:
    CD.CUD := mem;
    CD.recursion := CD.recursion - 1;
  exception
    when End_Error =>
      kind := Function_Unit;  --  Fake but valid value.
      Error (CD, err_unexpected_end_of_text);
    when others =>
      if needs_opening_a_stream then
        LD.cat.Close (full_file_name);
      end if;
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
