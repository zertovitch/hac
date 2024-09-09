with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

package body HAC_Sys.Parser.Tasking is

  use Compiler, Compiler.PCode_Emit, Co_Defs, Defs, Enter_Def, Helpers, PCode, Errors;
  use type HAC_Integer;

  ------------------------------------------------------------------
  -------------------------------------------------Task_Declaration-
  --  Hathorn
  procedure Task_Declaration (
    CD            : in out Co_Defs.Compiler_Data;
    FSys          :        Defs.Symset;
    Initial_Level :        Defs.Nesting_Level
  )
  is
    Level : Nesting_Level := Initial_Level;
    saveLineCount : constant Integer := CD.CUD.location.line;  --  Source line where Task appeared
    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;
    I, T0  : Integer;
    TaskID, TaskID_with_case : Alfa;
    task_block : Block_Data_Type;
    forward_id_idx : Natural;
    use type Alfa;
  begin
    InSymbol;
    if CD.Sy = BODY_Symbol then  --  Task Body
      InSymbol;
      I                := Locate_CD_Id (CD, Level);
      TaskID           := CD.IdTab (I).name;
      TaskID_with_case := CD.IdTab (I).name_with_case;
      CD.Blocks_Table (CD.IdTab (I).block_or_pkg_ref).SrcFrom := saveLineCount;  --  (* Manuel *)
      InSymbol;
      task_block.level                         := Level + 1;
      task_block.block_id_index                := I;
      task_block.entity                        := entree;
      task_block.is_main                       := False;
      task_block.previous_declaration_id_index := No_Id;
      Block (CD, FSys, False, task_block, TaskID, TaskID_with_case);
      Emit_1 (CD, k_Return_Call, Normal_Procedure_Call);
    else                         --  Task Specification
      if CD.Sy = IDent then
        TaskID           := CD.Id;
        TaskID_with_case := CD.Id_with_case;
      else
        Error (CD, err_identifier_missing);
        CD.Id            := Empty_Alfa;
        TaskID           := Empty_Alfa;
        TaskID_with_case := Empty_Alfa;
      end if;
      CD.Tasks_Definitions_Count := CD.Tasks_Definitions_Count + 1;
      if CD.Tasks_Definitions_Count > TaskMax then
        Fatal (TASKS);  --  Exception is raised there.
      end if;
      Enter_Prefixed (CD, Level, TaskID, TaskID_with_case, tache, forward_id_idx);
      CD.Tasks_Definitions_Table (CD.Tasks_Definitions_Count) := CD.Id_Count;
      Enter_Block (CD, CD.Id_Count);
      CD.IdTab (CD.Id_Count).block_or_pkg_ref := CD.Blocks_Count;
      InSymbol;
      if CD.Sy = Semicolon then
        InSymbol;  --  Task with no entries
      else  --  Parsing the Entry specs
        Need (CD, IS_Symbol, err_IS_missing);
        if Level = nesting_level_max then
          Fatal (LEVELS);  --  Exception is raised there.
        end if;
        Level              := Level + 1;
        CD.Display (Level) := CD.Blocks_Count;
        while CD.Sy = ENTRY_Symbol loop
          InSymbol;
          if CD.Sy /= IDent then
            Error (CD, err_identifier_missing);
            CD.Id := Empty_Alfa;
          end if;
          CD.Entries_Count := CD.Entries_Count + 1;
          if CD.Entries_Count > entry_table_max then
            Fatal (ENTRIES);  --  Exception is raised there.
          end if;
          Enter_Prefixed (CD, Level, CD.Id, CD.Id_with_case, entree, forward_id_idx);
          CD.Entries_Table (CD.Entries_Count) := CD.Id_Count;  --  point to identifier table location
          T0                                  := CD.Id_Count;  --  of TaskID
          InSymbol;
          task_block.level                         := Level + 1;
          task_block.block_id_index                := CD.Id_Count;
          task_block.entity                        := entree;
          task_block.is_main                       := False;
          task_block.previous_declaration_id_index := No_Id;
          Block (CD, FSys, False, task_block,
                 CD.IdTab (CD.Id_Count).name, CD.IdTab (CD.Id_Count).name_with_case);
          CD.IdTab (T0).adr_or_sz := HAC_Integer (CD.Tasks_Definitions_Count);
          if CD.Sy = Semicolon then
            InSymbol;
          else
            Error (CD, err_semicolon_missing);
          end if;
        end loop;  --  while CD.Sy = ENTRY_Symbol

        Level := Level - 1;
        Need_END_Symbol (CD);
        if CD.Sy = IDent and then CD.Id = TaskID then
          InSymbol;
        else
          Error_then_Skip (CD, Semicolon, err_incorrect_name_after_END);
        end if;
        Need_Semicolon_after_Declaration (CD, FSys);
      end if;
    end if;
    pragma Assert (Level = Initial_Level);
  end Task_Declaration;

end HAC_Sys.Parser.Tasking;
