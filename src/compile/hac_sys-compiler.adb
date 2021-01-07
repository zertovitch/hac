with HAC_Sys.Compiler.Library,
     HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Modularity,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.UErrors;

with Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Text_IO;

package body HAC_Sys.Compiler is

  use VStrings_Pkg;

  procedure Set_Source_Stream (
    CD         : in out Compiler_Data;
    s          : access Ada.Streams.Root_Stream_Type'Class;
    file_name  : in     String;       --  Can be a virtual name (editor title, zip entry)
    start_line : in     Natural := 0  --  We could have a shebang or other Ada sources before
  )
  is
  begin
    CD.compiler_stream  := Source_Stream_Access (s);
    CD.source_file_name := To_VString (file_name);
    CD.Line_Count       := start_line;
  end Set_Source_Stream;

  function Get_Current_Source_Name (CD : Compiler_Data) return String is
  begin
    return Defs.To_String (CD.source_file_name);
  end Get_Current_Source_Name;

  procedure Set_Error_Pipe (
    CD   : in out Compiler_Data;
    pipe :        Smart_error_pipe
  )
  is
  begin
    CD.error_pipe := pipe;
  end Set_Error_Pipe;

  procedure Init (CD : out Compiler_Data) is
  begin
    --  Array and block tables are clearly 1-based
    CD.Arrays_Count := 0;
    CD.Blocks_Count := 0;
    CD.Float_Constants_Count := 0;
    --  Identifiers
    CD.Id_Count := 0;
    --  Strings
    CD.Strings_Table_Top := CD.Strings_Constants_Table'First;
    --  Tasks, Entries
    CD.Tasks_Definitions_Count := 0;
    CD.Entries_Count := 0;
    --  Location Counter (in output code)
    CD.LC   := 0;
    CD.CMax := CDMax;
    --  Current block name for debugging of HAC programs.
    CD.Full_Block_Id := Universe;
    --
    CD.Main_Program_ID           := Empty_Alfa;
    CD.Main_Program_ID_with_case := Empty_Alfa;
    --
    --  Scanner data
    --
    CD.CH := ' ';
    CD.CC := 0;
    CD.LL := 0;
    CD.syStart := 1;
    CD.syEnd   := 1;
    Scanner.InSymbol (CD);
    --
    CD.Err_Count := 0;
    CD.Errs      := error_free;
    --
    CD.lib_map.Clear;
    CD.Display (0) := 0;  --  Added 7-Dec-2009
  end Init;

  --  Print_Tables is for debugging purposes.
  --
  procedure Print_Tables (CD : in Compiler_Data) is
    use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Strings.Fixed;
    --
    procedure Show_Padded (n : String; t : Positive) is
    begin
      Put (CD.comp_dump, "  " & n & Integer'Max (0, t - n'Length) * ' ');
    end Show_Padded;
  begin
    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump,
       " Identifiers" & (Alng - 6) * ' ' & "Link  Object                        " &
       "TYP              Ref  Norm Lvl  Adr"
    );
    Put_Line (CD.comp_dump,
       (Alng + Entity_Kind'Width + Typen'Width + Boolean'Width + 29) * '-'
    );
    --  We list all definitions, starting
    --  from Main (last Id of the "zero block" / standard).
    --
    for I in 1 .. CD.Id_Count loop
      declare
        r : IdTabEntry renames CD.IdTab (I);
      begin
        Put (CD.comp_dump, I, 4);
        Show_Padded (To_String (r.Name_with_case), Alng);
        Put (CD.comp_dump, r.Link, 4);
        Show_Padded (Entity_Kind'Image (r.Entity), Entity_Kind'Width);
        Show_Padded (Typen'Image (r.xTyp.TYP), Typen'Width);
        Put (CD.comp_dump, r.xTyp.Ref, 5);
        Show_Padded (Boolean'Image (r.Normal), Boolean'Width);
        Put (CD.comp_dump, Integer (r.LEV), 3);
        Put (CD.comp_dump, r.Adr_or_Sz, 5);
        New_Line (CD.comp_dump);
      end;
    end loop;

    New_Line (CD.comp_dump);
    Put_Line (CD.comp_dump, " Tasks       Block#");
    for I in 0 .. CD.Tasks_Definitions_Count loop
      Put (CD.comp_dump, I, 4);
      Put (CD.comp_dump, ' ');
      Put (CD.comp_dump, To_String (CD.IdTab (CD.Tasks_Definitions_Table (I)).Name) & "  ");
      Put (CD.comp_dump, CD.IdTab (CD.Tasks_Definitions_Table (I)).Block_Ref);
      New_Line (CD.comp_dump);
    end loop;

    New_Line (CD.comp_dump);

    if CD.Entries_Count > 0 then
      Put (CD.comp_dump, " Entries ");
      New_Line (CD.comp_dump);
      for I in 1 .. CD.Entries_Count loop
        Put (CD.comp_dump, I, 4);
        Put (CD.comp_dump,
             ' ' & To_String (CD.IdTab (CD.Entries_Table (I)).Name) & " in Task " &
             To_String (CD.IdTab (
               CD.Tasks_Definitions_Table (CD.IdTab (CD.Entries_Table (I)).Adr_or_Sz)
             ).Name)
        );
        New_Line (CD.comp_dump);
      end loop;
      New_Line (CD.comp_dump);
    end if;

    Put_Line (CD.comp_dump, " Blocks" & Alng * ' ' & "  Last_ID LPar PSze Vsze");
    --  There is a hidden block #0, "the Universe", with Standard
    for I in 0 .. CD.Blocks_Count loop
      declare
        r : BTabEntry renames CD.Blocks_Table (I);
      begin
        Put (CD.comp_dump, I, 4);
        Show_Padded (To_String (r.Id), Alng);
        Put (CD.comp_dump, r.Last_Id_Idx, 10);
        Put (CD.comp_dump, r.Last_Param_Id_Idx, 5);
        Put (CD.comp_dump, r.PSize, 5);
        Put (CD.comp_dump, r.VSize, 5);
        New_Line (CD.comp_dump);
      end;
    end loop;

    New_Line (CD.comp_dump);

    if CD.Arrays_Count > 0 then
      Put_Line (CD.comp_dump, " Arrays    Xtyp Etyp Eref  Low High ELSZ Size");
      for I in 1 .. CD.Arrays_Count loop
        declare
          r : ATabEntry renames CD.Arrays_Table (I);
        begin
          Put (CD.comp_dump, I, 4);
          Put (CD.comp_dump, Typen'Image (r.Index_xTyp.TYP) & "   " &
                             Typen'Image (r.Element_xTyp.TYP));
          Put (CD.comp_dump, r.Element_xTyp.Ref, 5);
          Put (CD.comp_dump, r.Low, 5);
          Put (CD.comp_dump, r.High, 5);
          Put (CD.comp_dump, r.Element_Size, 5);
          Put (CD.comp_dump, r.Array_Size, 5);
          New_Line (CD.comp_dump);
        end;
      end loop;
    end if;
  end Print_Tables;

  ---------------------------------------------------------------------------

  procedure Compile_Main (
    CD                 : in out Compiler_Data;
    asm_dump_file_name :        String  := "";  --  Assembler output of compiled object code
    cmp_dump_file_name :        String  := "";  --  Compiler dump
    listing_file_name  :        String  := "";  --  Listing of source code with details
    var_map_file_name  :        String  := ""   --  Output of variables (map)
  )
  is
    use Ada.Text_IO, Parser.Helpers, PCode, UErrors;

    asm_dump : File_Type;
    map_file : File_Type;

    procedure Dump_Asm is
    begin
      if asm_dump_file_name /= "" then
        Create (asm_dump, Out_File, asm_dump_file_name);
        Dump (
          CD.ObjCode (CD.ObjCode'First .. CD.LC - 1),  --  Dump only compiled part.
          CD.Strings_Constants_Table,
          CD.Float_Constants_Table,
          asm_dump
        );
        Close (asm_dump);
      end if;
    end Dump_Asm;

  begin  --  Compile
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

    Library.Apply_WITH_Standard (CD);
    Library.Apply_USE_Clause (
      CD, Library.Library_Level,
      Locate_Identifier (CD, To_Alfa ("STANDARD"), 0)
    );

    Parser.Modularity.Context_Clause (CD);  --  Parse the "with"'s and "use"'s.

    if CD.comp_dump_requested then
      Put_Line (CD.comp_dump, "Compiler: check for main procedure");
    end if;

    if CD.Sy /= PROCEDURE_Symbol then
      Error (CD, err_missing_a_procedure_declaration, "");  --  PROCEDURE Name IS
    else
      Scanner.InSymbol (CD);
      if CD.Sy /= IDent then
        Error (CD, err_identifier_missing);
      else
        CD.Main_Program_ID           := CD.Id;
        CD.Main_Program_ID_with_case := CD.Id_with_case;
        Scanner.InSymbol (CD);
      end if;
    end if;

    if CD.comp_dump_requested then
      Put_Line (CD.comp_dump, "Compiler: main procedure is " & To_String (CD.Main_Program_ID));
    end if;

    Library.Enter_Built_In (CD, To_String (CD.Main_Program_ID), Prozedure, NOTYP, 0);
    CD.Main_Proc_Id_Index := CD.Id_Count;

    CD.Blocks_Table (0) :=  --  Block Table Entry for Standard [was Main, 1]
     (Id                => To_Alfa ("-- Standard Definitions (The Universe)"),
      Last_Id_Idx       => CD.Id_Count,
      Last_Param_Id_Idx => 1,
      PSize             => 0,
      VSize             => 0,
      SrcFrom           => CD.Line_Count,
      SrcTo             => CD.Line_Count);

    CD.Tasks_Definitions_Table (0) := CD.Id_Count;  --  { Task Table Entry }

    --  Start Compiling
    Parser.Block (
      CD, Block_Begin_Symbol + Statement_Begin_Symbol,
      False, False, 1,
      CD.Id_Count,
      CD.IdTab (CD.Id_Count).Name,
      CD.Main_Program_ID_with_case
    );
    --  Main procedure is parsed.
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
    CD.Blocks_Table (1).SrcTo := CD.Line_Count;  --(* Manuel : terminate source *)

    if CD.listing_requested then
      Close (CD.listing);
    end if;

    if CD.Errs /= error_free then
      Compilation_Errors_Summary (CD);
    end if;

    if CD.comp_dump_requested then
      Print_Tables (CD);
      Close (CD.comp_dump);
    end if;

    if var_map_file_name /= "" then
      Create (map_file, Out_File, var_map_file_name);
      Put_Line (map_file, "  -* Symbol Table *-");
      New_Line (map_file);
      Put_Line (map_file, "  LOC  Name       scope");
      Put_Line (map_file, "------------------------");
      New_Line (map_file);
      for Tx in CD.Blocks_Table (0).Last_Id_Idx + 1 .. CD.Id_Count loop
        if CD.IdTab (Tx).Entity = Variable then
          if CD.IdTab (Tx).xTyp.TYP /= NOTYP then
            Ada.Integer_Text_IO.Put (map_file, CD.IdTab (Tx).Adr_or_Sz, 4);
            Put (map_file, To_String (CD.IdTab (Tx).Name) & "   ");
          end if;
          if CD.IdTab (Tx).LEV = 1 then
            Put (map_file, " Global(");
          else
            Put (map_file, " Local (");
          end if;
          Put (map_file, Nesting_level'Image (CD.IdTab (Tx).LEV));
          Put (map_file, ')');
          New_Line (map_file);
        end if;
      end loop;
      New_Line (map_file);
      Close (map_file);
    end if;

    Dump_Asm;

  exception
    when End_Error =>
      Error (CD, err_unexpected_end_of_text);
    when Compilation_abandoned =>
      --  Just too many errors...
      Compilation_Errors_Summary (CD);
      if CD.comp_dump_requested then
        Print_Tables (CD);
        Close (CD.comp_dump);
      end if;
      Dump_Asm;
  end Compile_Main;

  function Unit_Compilation_Successful (CD : Compiler_Data) return Boolean is
  begin
    return CD.Err_Count = 0;
  end Unit_Compilation_Successful;

  function Unit_Object_Code_Size (CD : Compiler_Data) return Natural is
  begin
    return CD.LC;
  end Unit_Object_Code_Size;

  function Maximum_Object_Code_Size return Natural is
  begin
    return Defs.CDMax;
  end Maximum_Object_Code_Size;

end HAC_Sys.Compiler;
