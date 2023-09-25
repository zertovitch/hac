with HAC_Sys.Co_Defs,
     HAC_Sys.Compiler.PCode_Emit,
     HAC_Sys.Defs,
     HAC_Sys.Multi_Precision_Integers,
     HAC_Sys.Parser.Expressions,
     HAC_Sys.Parser.Helpers,
     HAC_Sys.Parser.Ranges,
     HAC_Sys.PCode,
     HAC_Sys.Scanner,
     HAC_Sys.Errors;

procedure HAC_Sys.Parser.Statements.CASE_Statement  --  Ada RM 5.4
  (CD         : in out Co_Defs.Compiler_Data;
   FSys_St    :        Defs.Symset;
   Block_Data : in out Block_Data_Type)
is
  use Defs, Co_Defs, Compiler.PCode_Emit, Errors, Expressions, Helpers, PCode;
  use type HAC_Integer;

  type CASE_Label_Value is record
    value_1, value_2 : HAC_Integer;  --  value of a choice in a CASE statement
    LC               : Index;        --  instruction address
    Is_others        : Boolean;
  end record;

  CaseTab : array (1 .. Cases_Max) of CASE_Label_Value;
  ExitTab : array (1 .. Cases_Max) of Integer;

  X : Exact_Subtyp;
  choice_counter : Integer := 0;  --  This counts the various choices separated by '|'.
  exit_counter   : Integer := 0;  --  This will correspond to the number of "=>".
  LC1 : Integer;
  WHEN_OTHERS_flag : Boolean := False;

  use Multi_Precision_Integers;
  subtype Choice_Count_Type is Multi_Int (4);
  parsed_choices : Choice_Count_Type;

  procedure InSymbol is begin Scanner.InSymbol (CD); end InSymbol;

  function Count_Choice_Values (Low, High : HAC_Integer) return Choice_Count_Type is
    result : Choice_Count_Type;
  begin
    pragma Assert (Low <= High);
    Fill (result, Multi (High) - Multi (Low) + 1);
    return result;
  end Count_Choice_Values;

  procedure Discrete_Choice is  --  Ada RM 3.8.1 (5)
    label_1, label_2 : Constant_Rec;
    K : Integer;
    choice_symbol_set : constant Symset := FSys_St + Alt_Finger_THEN + Range_Double_Dot_Symbol;
  begin
    Static_Scalar_Expression (CD, Block_Data.level, choice_symbol_set, label_1);
    if CD.Sy = Range_Double_Dot_Symbol then
      --  !!  To do: non-explicit ranges, like a subtype name, a 'Range, ... .
      --      Ranges.Static_Range.
      InSymbol;
      Static_Scalar_Expression (CD, Block_Data.level, choice_symbol_set, label_2);
      if label_2.TP /= label_1.TP then
        Type_Mismatch (
          CD, err_case_label_not_same_type_as_case_clause,
          Found    => label_2.TP,
          Expected => label_1.TP
        );
      end if;
    else
      label_2 := label_1;
    end if;
    if Exact_Typ (label_1.TP) /= Exact_Typ (X) then
      Type_Mismatch (
        CD, err_case_label_not_same_type_as_case_clause,
        Found    => label_1.TP,
        Expected => X
      );
    elsif choice_counter = Cases_Max then
      Fatal (Case_Labels);  --  Exception is raised there.
    else
      if        (label_1.I not in X.Discrete_First .. X.Discrete_Last)
        or else (label_2.I not in X.Discrete_First .. X.Discrete_Last)
      then
        Error (CD, err_choice_out_of_range, severity => minor);
      end if;
      choice_counter := choice_counter + 1;
      CaseTab (choice_counter) :=
        (value_1   => label_1.I,
         value_2   => label_2.I,
         LC        => CD.LC,
         Is_others => False);
      K := 0;
      loop
        K := K + 1;
        --  Detect any range overlap:
        exit when
          Ranges.Do_Ranges_Overlap (label_1.I, label_2.I, CaseTab (K).value_1, CaseTab (K).value_2);
      end loop;
      if K < choice_counter then
        Error (CD, err_duplicate_case_choice_value);
      end if;
      --  Since single choices or ranges do not overlap,
      --  we can simply add the number of covered values in order to check
      --  at the end that everything is covered.
      Fill (parsed_choices, parsed_choices + Count_Choice_Values (label_1.I, label_2.I));
    end if;
  end Discrete_Choice;

  procedure WHEN_Discrete_Choice_List is
  begin
    pragma Assert (CD.Sy = WHEN_Symbol);  --  This subprogram is called only on WHEN_Symbol.
    InSymbol;  --  Consume `WHEN`
    --  Here, a discrete_choice_list (Ada RM 3.8.1 (4)) following WHEN.
    if Constant_Definition_Begin_Symbol (CD.Sy) then
      if WHEN_OTHERS_flag then  --  Normal choice list *atfer* the "others" choice.
        Error (CD, err_case_others_alone_last);
      end if;
      Discrete_Choice;
      while CD.Sy = Alt loop
        InSymbol;  --  Consume '|' symbol.
        if CD.Sy = OTHERS_Symbol then  --  "others" mixed with normal choices.
          Error (CD, err_case_others_alone_last);
        else
          Discrete_Choice;
        end if;
      end loop;
    elsif CD.Sy = OTHERS_Symbol then        -- Hathorn
      if WHEN_OTHERS_flag then  --  Duplicate "others".
        Error (CD, err_case_others_alone_last);
      end if;
      WHEN_OTHERS_flag := True;
      if choice_counter = Cases_Max then
        Fatal (Case_Labels);  --  Exception is raised there.
      end if;
      choice_counter := choice_counter + 1;
      CaseTab (choice_counter) :=
        (value_1 | value_2 => 0,
         LC                => CD.LC,
         Is_others         => True);
      InSymbol;
    end if;
    if CD.Sy = THEN_Symbol then  --  Mistake happens when converting IF statements to CASE.
      Error (CD, err_THEN_instead_of_Arrow, severity => major);
      InSymbol;
    else
      Need (CD, Finger, err_FINGER_missing);
    end if;
    Sequence_of_Statements (CD, END_WHEN, Block_Data);
    exit_counter := exit_counter + 1;
    ExitTab (exit_counter) := CD.LC;
    Emit (CD, k_Jump);
  end WHEN_Discrete_Choice_List;

  procedure Check_Coverage is
    expected_choices, difference : Choice_Count_Type;
  begin
    pragma Assert (Choice_Count_Type'Size >= HAC_Integer'Size);
    Fill (expected_choices, Count_Choice_Values (X.Discrete_First, X.Discrete_Last));
    Fill (difference, expected_choices - parsed_choices);
    if Equal (difference, 0) then
      if WHEN_OTHERS_flag then
        if CD.remarks (note_redundant_construct) then
          Note
            (CD,
             note_redundant_construct,
             """when others"" is redundant here: all values" &
             " are already explicitly covered");
        end if;
      end if;
    else
      pragma Assert (difference > 0);
      --  ^ NB: if the difference was negative, there would be more parsed
      --    choices than possible choices. Thus at least one choice
      --    would be out of range. But that error would have been detected
      --    on parsing and this procedure would not have been called.
      if not WHEN_OTHERS_flag then
        Error
          (CD, err_choices_not_covered,
           (if difference > 99 then ""  --  Too many omissions for display...
            elsif Equal (difference, 1) then ": one case is missing"
            else ":" & Basic_Int'Image (Basic (difference)) &
              " cases are missing"),
           severity => minor);
      end if;
    end if;
  end Check_Coverage;

begin  --  CASE_Statement
  pragma Assert (CD.Sy = CASE_Symbol);
  Fill (parsed_choices, 0);
  InSymbol;
  Expression (CD, Block_Data.level, FSys_St + Colon_Comma_IS_OF, X);
  if not Discrete_Typ (X.TYP) then
    Error (CD, err_bad_type_for_a_case_statement);
  end if;
  LC1 := CD.LC;
  Emit (CD, k_CASE_Switch);
  case CD.Sy is
    when IS_Symbol =>
      InSymbol;
    when OF_Symbol =>
      Error (CD, err_OF_instead_of_IS);
      --  ^ Common mistake by Pascal programmers.
      --  Historical note: "OF" and not "IS" was expected by SmallAda!
      --  For instance, "case x OF  when 1 => ..."
      InSymbol;
    when others =>
      Error (CD, err_IS_missing);
  end case;
  if CD.Sy /= WHEN_Symbol then
    Error (CD, err_WHEN_missing, severity => major);
  end if;
  loop
    --  All cases are parsed in this loop.
    WHEN_Discrete_Choice_List;
    exit when CD.Sy /= WHEN_Symbol;
  end loop;
  if CD.error_count + CD.minor_error_count = 0 then
    Check_Coverage;
  end if;
  CD.ObjCode (LC1).Y := Operand_2_Type (CD.LC);
  --  ^ Set correct address for k_CASE_Switch above.
  --  This is the address of the following bunch of
  --  instructions.

  --  Output the case table as (CASE_Any_Choice, k_CASE_Match_Jump)
  --  instruction pairs:
  for K in 1 .. choice_counter loop
    if CaseTab (K).Is_others then
      Emit (CD, k_CASE_Choice_Others);
    elsif CaseTab (K).value_1 = CaseTab (K).value_2 then
      Emit_1 (CD, k_CASE_Choice_Value, CaseTab (K).value_1);
    else
      Emit_2 (CD, k_CASE_Choice_Range, CaseTab (K).value_1, CaseTab (K).value_2);
    end if;
    Emit_1 (CD, k_CASE_Match_Jump, Operand_2_Type (CaseTab (K).LC));
  end loop;
  --  The following is for having the interpreter exiting the k_CASE_Choice_Data loop.
  --  Note: the k_CASE_No_Choice_Found allowed to check a missing "when others"
  --  at run-time. Now this check is done at compile-time by Check_Coverage.
  Emit (CD, k_CASE_No_Choice_Found);
  --
  for K in 1 .. exit_counter loop
    --  Patch k_Jump addresses to the instruction coming after "END CASE;" :
    CD.ObjCode (ExitTab (K)).Y := Operand_2_Type (CD.LC);
  end loop;
  Need (CD, END_Symbol,  err_END_missing);           --  END (CASE)
  Need (CD, CASE_Symbol, err_missing_closing_CASE);  --  (END) CASE
end HAC_Sys.Parser.Statements.CASE_Statement;
