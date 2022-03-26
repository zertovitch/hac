with HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Parser.Enter_Def,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Packages,
     HAC_Sys.Parser.Statements,
     HAC_Sys.Parser.Tasking,
     HAC_Sys.Parser.Const_Var,
     HAC_Sys.Parser.Type_Def,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

with HAL;

package body HAC_Sys.Parser is

  ------------------------------------------------------------------
  ------------------------------------------------------------Block-

  procedure Block (
    CD                   : in out Co_Defs.Compiler_Data;
    FSys                 :        Defs.Symset;
    Is_a_block_statement :        Boolean;        --  5.6 Block Statements
    Initial_Block_Data   :        Block_Data_Type;
    Block_Id             :        Defs.Alfa;      --  Name of this block (if any)
    Block_Id_with_case   :        Defs.Alfa
  )
  is
    use Co_Defs, Defs, Enter_Def, Errors, Helpers, PCode;
    use type HAC_Integer;
    --
    block_data : Block_Data_Type := Initial_Block_Data;
    subprogram_block_index : Integer;  --  Was: PRB

    procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;

    ------------------------------------------------------------------
    --------------------------------------------Formal_Parameter_List-
    procedure Formal_Parameter_List is
      Sz, X, T0 : Integer;
      ValParam  : Boolean;
      xTP       : Exact_Subtyp := Undefined;
    begin
      InSymbol;  --  Consume '(' symbol.
      Sz := 0;
      Test (CD, IDent_Set, FSys + RParent, err_identifier_missing, stop_on_error => True);
      --
      while CD.Sy = IDent loop
        T0 := CD.Id_Count;
        Enter_Variables (CD, block_data.level);
        --
        if CD.Sy = Colon then  --  The ':'  in  "function F (x, y : in Real) return Real;"
          InSymbol;
          if CD.Sy = IN_Symbol then
            InSymbol;
          end if;
          if block_data.is_a_function then  --  If I am a function, no InOut parms allowed
            ValParam := True;
          elsif CD.Sy /= OUT_Symbol then
            ValParam := True;
          else
            InSymbol;
            ValParam := False;
          end if;
          if CD.Sy = IDent then
            X := Locate_Identifier (CD, CD.Id, block_data.level);
            InSymbol;
            if X = CD.String_Id_Index then
              --  We could pass string literals as "in" parameter
              --  if we replaced String_Literals by a record wrapping
              --  the string length and the index into the string table.
              Error (CD, err_string_not_supported_as_parameter, severity => major);
            elsif X /= No_Id then
              if CD.IdTab (X).entity = TypeMark then
                xTP := CD.IdTab (X).xtyp;
                if ValParam then
                  Sz := CD.IdTab (X).adr_or_sz;
                else
                  Sz := 1;
                end if;
              else
                Error (CD, err_missing_a_type_identifier, severity => major);
              end if;
            end if;  --  X /= No_Id
          else
            Error (CD, err_identifier_missing);
          end if;
          Test (CD, Comma_IDent_RParent_Semicolon, FSys, err_semicolon_missing, stop_on_error => True);
          while T0 < CD.Id_Count loop
            T0 := T0 + 1;
            declare
              r : IdTabEntry renames CD.IdTab (T0);
            begin
              r.xtyp      := xTP;
              r.normal    := ValParam;
              r.read_only := ValParam;
              r.adr_or_sz := block_data.data_allocation_index;
              r.lev       := block_data.level;
              block_data.data_allocation_index := block_data.data_allocation_index + Sz;
            end;
          end loop;  --  while T0 < CD.Id_Count
        else
          Error (CD, err_colon_missing, severity => major);
        end if;
        if CD.Sy /= RParent then
          Need_Semicolon (CD);
          Test (CD, IDent_Set, FSys + RParent, err_incorrectly_used_symbol);
        end if;
      end loop;  --  while Sy = IDent
      --
      if CD.Sy = RParent then
        InSymbol;
        Test (CD, After_Subprogram_Parameters, FSys, err_incorrectly_used_symbol);
      else
        Error (CD, err_closing_parenthesis_missing);
      end if;
    end Formal_Parameter_List;

    procedure Declarative_Part is
      ignored : Declaration_Kind;
    begin
      loop
        Test (  --  Added 17-Apr-2018 to avoid infinite loop on erroneous code
          CD, Declaration_Symbol + BEGIN_Symbol,
          Empty_Symset,
          err_incorrectly_used_symbol,
          stop_on_error => True  --  Exception is raised there if there is an error.
        );
        case CD.Sy is
          when IDent              => Const_Var.Var_Declaration (CD, FSys, block_data);
          when TYPE_Symbol |
               SUBTYPE_Symbol     => Type_Def.Type_Declaration (CD, block_data.level, FSys);
          when TASK_Symbol        => Tasking.Task_Declaration (CD, FSys, block_data.level);
          when USE_Symbol         => Packages.Use_Clause (CD, block_data.level);
          when PROCEDURE_Symbol |
               FUNCTION_Symbol    => Subprogram_Declaration_or_Body (CD, FSys, block_data.level, ignored);
          when others => null;
        end case;
        CD.Blocks_Table (subprogram_block_index).VSize := block_data.data_allocation_index;
        exit when CD.Sy = BEGIN_Symbol;
      end loop;
      Check_Incomplete_Definitions (CD, block_data.level);
    end Declarative_Part;

    procedure Statements_Part_Setup is
    begin
      block_data.max_data_allocation_index := block_data.data_allocation_index;
      CD.IdTab (block_data.block_id_index).adr_or_sz := CD.LC;
      Link_Forward_Declaration (CD, block_data.previous_declaration_id_index, block_data.block_id_index);
      --  Copy initialization (elaboration) ObjCode from end of ObjCode table
      for Init_Code_Idx in reverse CD.CMax + 1 .. CD.CMax + block_data.initialization_object_code_size loop
        CD.ObjCode (CD.LC) := CD.ObjCode (Init_Code_Idx);
        CD.LC              := CD.LC + 1;
      end loop;
      --  Restore CMax to the initial max.
      --  At lowest nesting level, it will be CDMax.
      --  For higher levels, it will be CDMax minus the sum of
      --  current values of ICode for all lower levels.
      CD.CMax := CD.CMax + block_data.initialization_object_code_size;
    end Statements_Part_Setup;

    procedure Statements_Part_Closing is
    begin
      CD.Blocks_Table (subprogram_block_index).SrcTo := CD.CUD.line_count;
    end Statements_Part_Closing;

    procedure Function_Result_Profile is
      I_Res_Type : Integer;
    begin
      if CD.Sy = RETURN_Symbol then
        InSymbol;  --  FUNCTION TYPE
        if CD.Sy = IDent then
          I_Res_Type := Locate_Identifier (CD, CD.Id, block_data.level);
          InSymbol;
          if I_Res_Type /= 0 then
            if CD.IdTab (I_Res_Type).entity /= TypeMark then
              Error (CD, err_missing_a_type_identifier, severity => major);
            elsif Standard_or_Enum_Typ (CD.IdTab (I_Res_Type).xtyp.TYP) then
              CD.IdTab (block_data.block_id_index).xtyp := CD.IdTab (I_Res_Type).xtyp;
            else
              Error (CD, err_bad_result_type_for_a_function, severity => major);
            end if;
          end if;
        else
          Error (CD, err_identifier_missing, severity => major);
        end if;
      else
        Error (CD, err_RETURN_missing, severity => major);
      end if;
    end Function_Result_Profile;

    Restore_Block_ID : constant HAL.VString := CD.Full_Block_Id;
    use HAL;

    procedure Check_ident_after_END is
      full_name : VString;
      --  ^ It can be a library unit name, like: "Parent_1.Child_3.Grandchild_5".
    begin
      pragma Assert (CD.Sy = IDent);
      loop
        full_name := full_name & To_String (CD.Id);
        InSymbol;
        exit when CD.Sy /= Period;
        full_name := full_name & '.';
        InSymbol;
        if CD.Sy /= IDent then
          Error (CD, err_identifier_missing);
        end if;
      end loop;
      if VStr_Pkg.To_String (full_name) /= To_String (Block_Id) then
        Error
          (CD, err_incorrect_name_after_END,
           hint => To_String (Block_Id_with_case),
           previous_symbol => True,
           --  ^ Ideally we would enclose the whole wrong full name, possibly on several lines.
           --  But it is correct on a single wrong identifier, the most frequent case.
           severity => minor
          );
      end if;
    end Check_ident_after_END;

    procedure Process_Spec is
    begin
      CD.IdTab (block_data.block_id_index).decl_kind := spec_unresolved;
      CD.IdTab (block_data.block_id_index).adr_or_sz := -1;
      --  ^ This invalid address will raise VM_Subprogram_Spec.
      Check_Duplicate_Specification (CD, block_data.previous_declaration_id_index, Block_Id_with_case);
      CD.Blocks_Table (subprogram_block_index).VSize := block_data.data_allocation_index;
      --
      if block_data.level > 1 then
        InSymbol;  --  Consume ';'
      end if;
      --  End of subprogram specification part (forward declaration).
      --  Body is declared later in the containing block or elsewhere in the library.
    end Process_Spec;

    procedure Process_Body is
    begin
      CD.IdTab (block_data.block_id_index).decl_kind := complete;
      Check_Spec_Body_Consistency
        (CD,
         block_data.previous_declaration_id_index,
         block_data.block_id_index,
         Block_Id_with_case);
      --
      if Is_a_block_statement then
        case CD.Sy is
          when DECLARE_Symbol => InSymbol;
          when BEGIN_Symbol   => null;
          when others         => raise Internal_error with "Unexpected " & KeyWSymbol'Image (CD.Sy);
        end case;
      elsif CD.Sy = IS_Symbol then  --  The "IS" in "procedure ABC (param : T_Type) IS"
        InSymbol;
      else
        Error (CD, err_IS_missing);
        return;
      end if;
      --
      if CD.Sy = NULL_Symbol and not Is_a_block_statement then
        --  RM 6.7 Null Procedures (Ada 2005)
        --  E.g.: "procedure Not_Yet_Done (a : Integer) is null;"
        InSymbol;  --  Consume NULL symbol.
        Statements_Part_Setup;
        if block_data.is_a_function then
          --  There are no null functions: what would be the result?
          Error (CD, err_no_null_functions);
        else
          null;  --  No statement -> no instruction, like for the NULL statement.
        end if;
        Statements_Part_Closing;
      else
        Declarative_Part;
        InSymbol;  --  Consume BEGIN symbol.
        Statements_Part_Setup;
        Statements.Sequence_of_Statements (CD, END_Set, block_data);
        Statements_Part_Closing;
        --
        if CD.Sy = END_Symbol then
          InSymbol;
        elsif CD.error_count > 0 then
          return;  --  At this point the program is already FUBAR.
        else
          Error (CD, err_END_missing);
          return;
        end if;
        --
        if CD.Sy = IDent then  --  Verify that the name after "end" matches the unit name.
          Check_ident_after_END;
        elsif Is_a_block_statement and Block_Id /= Empty_Alfa then
          --  No identifier after "end", but "end [label]" is required in this case.
          Error (CD, err_incorrect_name_after_END, hint => To_String (Block_Id_with_case));
        end if;
      end if;
      --
      if CD.Sy /= Semicolon then
        Error (CD, err_semicolon_missing);
        return;
      end if;
      --
      if block_data.level <= 1 or Is_a_block_statement then
        --  Time to count the minor errors as errors.
        CD.error_count := CD.error_count + CD.minor_error_count;
        CD.minor_error_count := 0;
      else
        InSymbol;  --  Consume ';' symbol after END [Subprogram_Id].
        Ignore_Extra_Semicolons (CD);
        --
        --  Now we have either another declaration,
        --  or BEGIN or, if it's a package, END  .
        Test (
          CD, FSys + Declaration_Symbol + BEGIN_Symbol + END_Symbol,
          Empty_Symset,
          err_incorrectly_used_symbol
        );
      end if;
    end Process_Body;

  begin  --  Block
    if CD.error_count > 0 then
      return;
    end if;
    if CD.Full_Block_Id = Universe then
      CD.Full_Block_Id := HAL.To_VString (To_String (Block_Id_with_case));
    else
      CD.Full_Block_Id := CD.Full_Block_Id & '.' & To_String (Block_Id_with_case);
    end if;
    block_data.data_allocation_index := 5;  --  Fixed area of the subprogram activation record.
    block_data.initialization_object_code_size := 0;
    if Is_a_block_statement then
      null;  --  We should be here with Sy = BEGIN_Symbol or Sy = DECLARE_Symbol.
    else
      Test (CD, Symbols_after_Subprogram_Identifier, FSys, err_incorrectly_used_symbol);
    end if;
    if CD.IdTab (block_data.block_id_index).block_ref > 0 then
      subprogram_block_index := CD.IdTab (block_data.block_id_index).block_ref;
    else
      Enter_Block (CD, block_data.block_id_index);
      subprogram_block_index := CD.Blocks_Count;
      CD.IdTab (block_data.block_id_index).block_ref := subprogram_block_index;
    end if;
    CD.Display (block_data.level) := subprogram_block_index;
    CD.IdTab (block_data.block_id_index).xtyp := Undefined;
    if CD.Sy = LParent then
      Formal_Parameter_List;
    end if;
    --
    if CD.error_count > 0 then
      return;
    end if;
    --
    CD.Blocks_Table (subprogram_block_index).Last_Param_Id_Idx := CD.Id_Count;
    CD.Blocks_Table (subprogram_block_index).PSize := block_data.data_allocation_index;
    --
    if block_data.is_a_function and not Is_a_block_statement then
      Function_Result_Profile;
    end if;
    --
    if CD.Sy = Semicolon then
      Process_Spec;
    else
      Process_Body;
    end if;
    CD.Full_Block_Id := Restore_Block_ID;
    if CD.error_count = 0 then
      pragma Assert (block_data.level = Initial_Block_Data.level);
    end if;
  end Block;

  procedure Subprogram_Declaration_or_Body (
    CD            : in out Co_Defs.Compiler_Data;
    FSys          : in     Defs.Symset;
    current_level : in     Defs.Nesting_level;
    kind          :    out Co_Defs.Declaration_Kind
  )
  is
    use Co_Defs, Compiler.PCode_Emit, Defs, Enter_Def, Errors, PCode;
    use type HAC_Integer;
    new_id_idx, old_id_idx : Natural;
    IsFun : constant Boolean := CD.Sy = FUNCTION_Symbol;
    sub_sub_prog_block_data : Block_Data_Type;
  begin
    Scanner.InSymbol (CD);
    if CD.Sy /= IDent then
      Error (CD, err_identifier_missing);
      CD.Id := Empty_Alfa;
    end if;
    declare
      id_subprog_with_case : constant Alfa := CD.Id_with_case;
    begin
      if IsFun then
        Enter (CD, current_level, CD.Id, id_subprog_with_case, Funktion, old_id_idx);
      else
        Enter (CD, current_level, CD.Id, id_subprog_with_case, Prozedure, old_id_idx);
      end if;
      --  NB: now old_id_idx, if different than No_Id, points to the
      --  eventual previous declaration of the subprogram with that name.
      Scanner.InSymbol (CD);
      sub_sub_prog_block_data.level                         := current_level + 1;
      sub_sub_prog_block_data.block_id_index                := CD.Id_Count;
      sub_sub_prog_block_data.is_a_function                 := IsFun;
      sub_sub_prog_block_data.previous_declaration_id_index := old_id_idx;
      new_id_idx := CD.Id_Count;
      Block (CD, FSys, False, sub_sub_prog_block_data,
             CD.IdTab (new_id_idx).name, id_subprog_with_case);
      kind := CD.IdTab (new_id_idx).decl_kind;
      if kind = complete then
        if IsFun then
          Emit_1 (CD, k_Exit_Function, End_Function_without_Return);
        else
          Emit_1 (CD, k_Exit_Call, Normal_Procedure_Call);
        end if;
      end if;
    end;
  end Subprogram_Declaration_or_Body;

end HAC_Sys.Parser;
