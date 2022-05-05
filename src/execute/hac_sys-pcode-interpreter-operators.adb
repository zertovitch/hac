with HAC_Sys.PCode.Interpreter.Exceptions;

with HAL;

with Ada.Calendar,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Exceptions,
     Ada.Numerics.Float_Random,
     Ada.Strings;

package body HAC_Sys.PCode.Interpreter.Operators is

  procedure Do_Unary_Operator (ND : in out Interpreter_Data) is
    Curr_TCB_Top : Integer renames ND.TCB (ND.CurTask).T;
    X : General_Register renames ND.S (Curr_TCB_Top);
    H1 : Defs.Index;
    use type Defs.HAC_Float, Defs.HAC_Integer;
    I_to_F : Defs.HAC_Float;
  begin
    case Unary_Operator_Opcode (ND.IR.F) is
      when k_Dereference         => X := ND.S (Defs.Index (X.I));  --  "[T] := ([T].I).all"
      when k_NOT_Boolean         => X.I := Boolean'Pos (not Boolean'Val (X.I));
      when k_Unary_MINUS_Integer => X.I := -X.I;
      when k_Unary_MINUS_Float   => X.R := -X.R;
      when k_Integer_to_Float =>
        H1 := Curr_TCB_Top - Defs.Index (ND.IR.Y);
        I_to_F := Defs.HAC_Float (ND.S (H1).I);
        ND.S (H1) := GR_Real (I_to_F);
    end case;
  end Do_Unary_Operator;

  procedure Do_Binary_Operator (ND : in out Interpreter_Data) is
    Curr_TCB_Top : Integer renames ND.TCB (ND.CurTask).T;
    X : General_Register renames ND.S (Curr_TCB_Top - 1);  --  X = [T-1]
    Y : General_Register renames ND.S (Curr_TCB_Top);      --  Y = [T]
    use HAL.VStr_Pkg;
    use type Defs.HAC_Float, Defs.HAC_Integer;
  begin
    --  We do  [T] <- ([T-1] operator [T])  and pop later.
    case Binary_Operator_Opcode (ND.IR.F) is
      when k_EQL_Float =>   X.I := Boolean'Pos (X.R =  Y.R);
      when k_NEQ_Float =>   X.I := Boolean'Pos (X.R /= Y.R);
      when k_LSS_Float =>   X.I := Boolean'Pos (X.R <  Y.R);
      when k_LEQ_Float =>   X.I := Boolean'Pos (X.R <= Y.R);
      when k_GTR_Float =>   X.I := Boolean'Pos (X.R >  Y.R);
      when k_GEQ_Float =>   X.I := Boolean'Pos (X.R >= Y.R);
      --
      when k_EQL_Integer => X.I := Boolean'Pos (X.I =  Y.I);
      when k_NEQ_Integer => X.I := Boolean'Pos (X.I /= Y.I);
      when k_LSS_Integer => X.I := Boolean'Pos (X.I <  Y.I);
      when k_LEQ_Integer => X.I := Boolean'Pos (X.I <= Y.I);
      when k_GTR_Integer => X.I := Boolean'Pos (X.I >  Y.I);
      when k_GEQ_Integer => X.I := Boolean'Pos (X.I >= Y.I);
      --
      when k_EQL_VString => X.I := Boolean'Pos (X.V =  Y.V);
      when k_NEQ_VString => X.I := Boolean'Pos (X.V /= Y.V);
      when k_LSS_VString => X.I := Boolean'Pos (X.V <  Y.V);
      when k_LEQ_VString => X.I := Boolean'Pos (X.V <= Y.V);
      when k_GTR_VString => X.I := Boolean'Pos (X.V >  Y.V);
      when k_GEQ_VString => X.I := Boolean'Pos (X.V >= Y.V);
      --
      when k_AND_Boolean => X.I := Boolean'Pos (Boolean'Val (X.I) and Boolean'Val (Y.I));
      when k_OR_Boolean  => X.I := Boolean'Pos (Boolean'Val (X.I) or  Boolean'Val (Y.I));
      when k_XOR_Boolean => X.I := Boolean'Pos (Boolean'Val (X.I) xor Boolean'Val (Y.I));
      --
      when k_ADD_Integer      => X.I := X.I + Y.I;
      when k_SUBTRACT_Integer => X.I := X.I - Y.I;
      when k_MULT_Integer     => X.I := X.I * Y.I;
      when k_DIV_Integer      =>
        if Y.I = 0 then raise VM_Division_by_0 with "(/)"; end if;
        X.I := X.I / Y.I;
      when k_MOD_Integer      =>
        if Y.I = 0 then raise VM_Division_by_0 with "(mod)"; end if;
        X.I := X.I mod Y.I;
      when k_REM_Integer      =>
        if Y.I = 0 then raise VM_Division_by_0 with "(rem)"; end if;
        X.I := X.I rem Y.I;
      when k_Power_Integer    => X.I := X.I ** Natural (Y.I);
      --
      when k_ADD_Float           => X.R := X.R + Y.R;
      when k_SUBTRACT_Float      => X.R := X.R - Y.R;
      when k_MULT_Float          => X.R := X.R * Y.R;
      when k_DIV_Float           => X.R := X.R / Y.R;
      when k_Power_Float         => X.R := X.R ** Y.R;
      when k_Power_Float_Integer => X.R := X.R ** Natural (Y.I);
    end case;
    Pop (ND);
  end Do_Binary_Operator;

  procedure Do_SF_Operator (CD : Compiler_Data; ND : in out Interpreter_Data) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    Top_Item : General_Register renames ND.S (Curr_TCB.T);
    temp : Defs.HAC_Float;
    temp_I : Defs.HAC_Integer;
    Idx, Len, From, To : Integer;
    C : Character;
    Code : constant SF_Code := SF_Code'Val (ND.IR.Y);
    use Defs, Exceptions;
    use Ada.Numerics.Float_Random,
        Ada.Strings;
    use type Defs.HAC_Float, Defs.HAC_Integer, HAL.Time, HAL.VString;
    Going : Direction;
    --
  begin
    case Code is
      when SF_Abs_Int   => Top_Item.I := abs (Top_Item.I);
      when SF_Abs_Float => Top_Item.R := abs (Top_Item.R);
      when SF_T_Val =>   --  Ord = Character'Val : RM 3.5.5 (5,7)
        if Top_Item.I not in Defs.OrdMinChar .. Defs.OrdMaxChar then
          raise VM_Out_of_Range with ": not in Character's range";
        end if;
      when SF_T_Pos =>   --  S'Pos : RM 3.5.5 (2)
        null;
      when SF_T_Succ => Top_Item.I := Top_Item.I + 1;  --  S'Succ : RM 3.5 (22)
      when SF_T_Pred => Top_Item.I := Top_Item.I - 1;  --  S'Pred : RM 3.5 (25)
      when SF_in_discrete_Interval =>
        --  SF_in_discrete_Interval (x, a, b) is equivalent to: `x in a .. b`
        Pop (ND, 2);
        --  [T] := [T] in [T+1] .. [T+2]
        ND.S (Curr_TCB.T).I :=
          Boolean'Pos (
            ND.S (Curr_TCB.T).I in ND.S (Curr_TCB.T + 1).I .. ND.S (Curr_TCB.T + 2).I
          );
      when SF_Round_Float_to_Int =>
        Top_Item.I := HAC_Integer (Top_Item.R);
      when SF_Trunc_Float_to_Int =>
        Top_Item.I := HAC_Integer (Defs.HAC_Float'Floor (Top_Item.R));
      when SF_Float_to_Duration =>
        Top_Item := GR_Duration (Duration (Top_Item.R));
      when SF_Duration_to_Float =>
        Top_Item := GR_Real (Defs.HAC_Float (Top_Item.Dur));
      when SF_Int_to_Duration =>
        Top_Item := GR_Duration (Duration (Top_Item.I));
      when SF_Duration_to_Int =>
        Top_Item.I := HAC_Integer (Top_Item.Dur);
      when SF_Sin =>    Top_Item.R := HAL.Sin (Top_Item.R);
      when SF_Cos =>    Top_Item.R := HAL.Cos (Top_Item.R);
      when SF_Exp =>    Top_Item.R := HAL.Exp (Top_Item.R);
      when SF_Log =>    Top_Item.R := HAL.Log (Top_Item.R);
      when SF_Sqrt =>   Top_Item.R := HAL.Sqrt (Top_Item.R);
      when SF_Arctan => Top_Item.R := HAL.Arctan (Top_Item.R);
      when SF_Random_Int =>
        loop
          temp := Defs.HAC_Float (Random (ND.Gen)) *
                  Defs.HAC_Float ((Top_Item.I + 1));
          temp_I := HAC_Integer (Defs.HAC_Float'Floor (temp));
          exit when temp_I < Top_Item.I + 1;
          --  ^ In extremely rare cases we have = Top_Item.I + 1.
        end loop;
        Top_Item.I := temp_I;
      when SF_Min_Int =>
        Pop (ND);
        --  [T] := Min ([T], [T+1]) :
        ND.S (Curr_TCB.T).I := HAC_Integer'Min (ND.S (Curr_TCB.T).I, ND.S (Curr_TCB.T + 1).I);
      when SF_Max_Int =>
        Pop (ND);
        --  [T] := Max ([T], [T+1]) :
        ND.S (Curr_TCB.T).I := HAC_Integer'Max (ND.S (Curr_TCB.T).I, ND.S (Curr_TCB.T + 1).I);
      when SF_Min_Float =>
        Pop (ND);
        --  [T] := Min ([T], [T+1]) :
        ND.S (Curr_TCB.T).R := HAC_Float'Min (ND.S (Curr_TCB.T).R, ND.S (Curr_TCB.T + 1).R);
      when SF_Max_Float =>
        Pop (ND);
        --  [T] := Max ([T], [T+1]) :
        ND.S (Curr_TCB.T).R := HAC_Float'Max (ND.S (Curr_TCB.T).R, ND.S (Curr_TCB.T + 1).R);
      when SF_String_to_VString =>   --  Unary "+", equivalent to the call To_VString (S)
        Idx := Integer (ND.S (Curr_TCB.T).I);      --  Index in the stack
        Len := Integer (ND.IR.X);                  --  Length of string
        ND.S (Curr_TCB.T) := GR_VString (Get_String_from_Stack (ND, Idx, Len));
      when SF_Literal_to_VString =>  --  Unary "+", equivalent to the call To_VString ("abc")
        Pop (ND);
        Len := Integer (ND.S (Curr_TCB.T).I);      --  Length of string
        Idx := Integer (ND.S (Curr_TCB.T + 1).I);  --  Index to string table
        ND.S (Curr_TCB.T) := GR_VString (CD.Strings_Constants_Table (Idx .. Idx + Len - 1));
      when SF_VString_to_String =>
        --  The type has just changed to Strings_as_VStrings at parser level.
        --  For the VM, it's a VString one way or the other.
        null;
      when SF_Char_to_VString =>
        --  We create a 1-character temporary String: (1 => Character'Val (...)) and
        --  convert it to a VString.
        ND.S (Curr_TCB.T) := GR_VString (HAL.To_VString ((1 => Character'Val (ND.S (Curr_TCB.T).I))));
      when SF_Two_VStrings_Concat =>
        Pop (ND);
        --  [T] := [T] & [T+1] :
        ND.S (Curr_TCB.T).V := ND.S (Curr_TCB.T).V & ND.S (Curr_TCB.T + 1).V;
      when SF_VString_Char_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T).V := ND.S (Curr_TCB.T).V & Character'Val (ND.S (Curr_TCB.T + 1).I);
      when SF_Char_VString_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T) :=
          GR_VString (Character'Val (ND.S (Curr_TCB.T).I) & ND.S (Curr_TCB.T + 1).V);
      when SF_LStr_VString_Concat =>
        --  Literal: 2 items, VString: 1 item. Total, 3 items folded into 1 item.
        Pop (ND, 2);
        Len := Integer (ND.S (Curr_TCB.T).I);      --  Length of string
        Idx := Integer (ND.S (Curr_TCB.T + 1).I);  --  Index to string table
        ND.S (Curr_TCB.T) :=
          GR_VString (CD.Strings_Constants_Table (Idx .. Idx + Len - 1) & ND.S (Curr_TCB.T + 2).V);
      when SF_VString_Int_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T).V := ND.S (Curr_TCB.T).V & HAL.To_VString (HAC_Image (ND.S (Curr_TCB.T + 1).I));
      when SF_Int_VString_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T) := GR_VString ((HAC_Image (ND.S (Curr_TCB.T).I)) & ND.S (Curr_TCB.T + 1).V);
      when SF_VString_Float_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T).V := HAL."&" (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).R);
      when SF_Float_VString_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T) :=
          GR_VString (HAL."&" (ND.S (Curr_TCB.T).R, ND.S (Curr_TCB.T + 1).V));
      when SF_VString_Duration_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T).V := HAL."&" (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).Dur);
      when SF_Duration_VString_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T) := GR_VString (HAL."&" (ND.S (Curr_TCB.T).Dur, ND.S (Curr_TCB.T + 1).V));
      when SF_VString_Boolean_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T).V := HAL."&" (ND.S (Curr_TCB.T).V, Boolean'Val (ND.S (Curr_TCB.T + 1).I));
      when SF_Boolean_VString_Concat =>
        Pop (ND);
        ND.S (Curr_TCB.T) := GR_VString (HAL."&" (Boolean'Val (ND.S (Curr_TCB.T).I), ND.S (Curr_TCB.T + 1).V));
      when SF_Element =>
        Pop (ND);
        --  [T] := Element ([T], [T+1]) :
        C := HAL.Element (ND.S (Curr_TCB.T).V, Integer (ND.S (Curr_TCB.T + 1).I));
        ND.S (Curr_TCB.T).I := Character'Pos (C);
      when SF_Length =>
        --  [T] := Length ([T]) :
        Len := HAL.Length (Top_Item.V);
        Top_Item.I := HAC_Integer (Len);
      when SF_Slice =>
        Pop (ND, 2);
        From := Integer (ND.S (Curr_TCB.T + 1).I);
        To   := Integer (ND.S (Curr_TCB.T + 2).I);
        if From < 1 then
          Raise_Standard (ND, VME_Constraint_Error, "Slice: Low is not positive:" &
            Integer'Image (From), True);
        end if;
        if To < 0 then
          Raise_Standard (ND, VME_Constraint_Error, "Slice: High is negative: " &
            Integer'Image (To), True);
        end if;
        Len := HAL.Length (ND.S (Curr_TCB.T).V);
        if From > Len + 1 then
          Raise_Standard (ND, VME_Index_Error,
            "Slice: Low is larger than Length (Source) + 1. See RM A.4.4 (101)", True);
        end if;
        if To > Len then
          Raise_Standard (ND, VME_Index_Error,
            "Slice: High is larger than Length (Source). See RM A.4.4 (101)", True);
        end if;
        --  [T] := Slice ([T], [T+1], [T+2]) :
        ND.S (Curr_TCB.T).V := HAL.To_VString (HAL.VStr_Pkg.Slice (ND.S (Curr_TCB.T).V, From, To));
      when SF_To_Lower_Char =>
        Top_Item.I := Character'Pos (HAL.To_Lower (Character'Val (Top_Item.I)));
      when SF_To_Upper_Char =>
        Top_Item.I := Character'Pos (HAL.To_Upper (Character'Val (Top_Item.I)));
      when SF_To_Lower_VStr =>
        Top_Item.V := HAL.To_VString (HAL.ACH.To_Lower (HAL.VStr_Pkg.To_String (Top_Item.V)));
      when SF_To_Upper_VStr =>
        Top_Item.V := HAL.To_VString (HAL.ACH.To_Upper (HAL.VStr_Pkg.To_String (Top_Item.V)));
      when SF_Index | SF_Index_Backward =>
        if Code = SF_Index then
          Going := Forward;
        else
          Going := Backward;
        end if;
        Pop (ND, 2);
        From := Integer (ND.S (Curr_TCB.T + 2).I);
        if From >= 1 then
          --  [T] := Index (Source: [T], Pattern: [T+1], From: [T+2], Going) :
          ND.S (Curr_TCB.T).I :=
            HAC_Integer (
              HAL.VStr_Pkg.Index (
                ND.S (Curr_TCB.T).V,
                HAL.VStr_Pkg.To_String (ND.S (Curr_TCB.T + 1).V),
                From,
                Going
              )
            );
        else
          --  [T] := Index (Source: [T], Pattern: [T+1], Going) :
          ND.S (Curr_TCB.T).I :=
            HAC_Integer (
              HAL.VStr_Pkg.Index (
                ND.S (Curr_TCB.T).V,
                HAL.VStr_Pkg.To_String (ND.S (Curr_TCB.T + 1).V),
                Going
              )
            );
        end if;
      when SF_Head =>
        Pop (ND);
        --  [T] := Head ([T], [T+1]) :
        ND.S (Curr_TCB.T).V :=
          HAL.VStr_Pkg.Head (ND.S (Curr_TCB.T).V, Natural (ND.S (Curr_TCB.T + 1).I));
      when SF_Tail =>
        Pop (ND);
        --  [T] := Tail ([T], [T+1]) :
        ND.S (Curr_TCB.T).V :=
          HAL.VStr_Pkg.Tail (ND.S (Curr_TCB.T).V, Natural (ND.S (Curr_TCB.T + 1).I));
      when SF_Head_Before_Match =>
        Pop (ND);
        --  [T] := Head_Before_Match ([T], [T+1]) :
        ND.S (Curr_TCB.T).V :=
          HAL.Head_Before_Match (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).V);
      when SF_Tail_After_Match =>
        Pop (ND);
        --  [T] := Tail_After_Match ([T], [T+1]) :
        ND.S (Curr_TCB.T).V :=
          HAL.Tail_After_Match (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).V);
      when SF_Starts_With =>
        Pop (ND);
        --  [T] := Starts_With ([T], [T+1]) :
        ND.S (Curr_TCB.T).I :=
          Boolean'Pos (HAL.Starts_With (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).V));
      when SF_Ends_With =>
        Pop (ND);
        --  [T] := Ends_With ([T], [T+1]) :
        ND.S (Curr_TCB.T).I :=
          Boolean'Pos (HAL.Ends_With (ND.S (Curr_TCB.T).V, ND.S (Curr_TCB.T + 1).V));
      when SF_Year =>
        ND.S (Curr_TCB.T).I := HAC_Integer (Ada.Calendar.Year (ND.S (Curr_TCB.T).Tim));
      when SF_Month =>
        ND.S (Curr_TCB.T).I := HAC_Integer (Ada.Calendar.Month (ND.S (Curr_TCB.T).Tim));
      when SF_Day =>
        ND.S (Curr_TCB.T).I := HAC_Integer (Ada.Calendar.Day (ND.S (Curr_TCB.T).Tim));
      when SF_Seconds =>
        ND.S (Curr_TCB.T) := GR_Duration (Ada.Calendar.Seconds (ND.S (Curr_TCB.T).Tim));
      when SF_Int_Times_Char =>
        Pop (ND);
        if ND.S (Curr_TCB.T).I < 0 then raise VM_Out_of_Range with ": negative value"; end if;
        --  [T] := [T] * [T+1] :
        ND.S (Curr_TCB.T) :=
          GR_VString (Natural (ND.S (Curr_TCB.T).I) * Character'Val (ND.S (Curr_TCB.T + 1).I));
      when SF_Int_Times_VStr =>
        Pop (ND);
        if ND.S (Curr_TCB.T).I < 0 then raise VM_Out_of_Range with ": negative value"; end if;
        --  [T] := [T] * [T+1] :
        ND.S (Curr_TCB.T) :=
          GR_VString (Natural (ND.S (Curr_TCB.T).I) * ND.S (Curr_TCB.T + 1).V);
      when SF_Trim_Left  => Top_Item.V := HAL.VStr_Pkg.Trim (Top_Item.V, Left);
      when SF_Trim_Right => Top_Item.V := HAL.VStr_Pkg.Trim (Top_Item.V, Right);
      when SF_Trim_Both  => Top_Item.V := HAL.VStr_Pkg.Trim (Top_Item.V, Both);
      --
      when SF_Time_Subtract =>
        Pop (ND);
        ND.S (Curr_TCB.T) := GR_Duration (ND.S (Curr_TCB.T).Tim - ND.S (Curr_TCB.T + 1).Tim);
      when SF_Duration_Subtract =>
        Pop (ND);
        ND.S (Curr_TCB.T).Dur := ND.S (Curr_TCB.T).Dur - ND.S (Curr_TCB.T + 1).Dur;
      when SF_Duration_Add =>
        Pop (ND);
        ND.S (Curr_TCB.T).Dur := ND.S (Curr_TCB.T).Dur + ND.S (Curr_TCB.T + 1).Dur;
      --
      when SF_Image_Ints      => Top_Item := GR_VString (HAC_Image (Top_Item.I));
      when SF_Image_Floats    => Top_Item := GR_VString (HAL.HAC_Image (Top_Item.R));
      when SF_Image_Times     => Top_Item := GR_VString (HAL.HAC_Image (Top_Item.Tim));
      when SF_Image_Durations => Top_Item := GR_VString (HAL.Image (Top_Item.Dur));
      --
      when SF_Integer_Value | SF_Value_Attribute_Ints =>
        begin
          Top_Item.I := HAC_Integer'Value (HAL.To_String (Top_Item.V));
        exception
          when E : Constraint_Error =>
            Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E), True);
        end;
      when SF_Float_Value | SF_Value_Attribute_Floats =>
        begin
          Top_Item := GR_Real (HAC_Float'Value (HAL.To_String (Top_Item.V)));
        exception
          when E : Constraint_Error =>
            Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E), True);
        end;
      --
      when SF_Image_Attribute_Ints   => Top_Item := GR_VString (HAC_Integer'Image (Top_Item.I));
      when SF_Image_Attribute_Floats => Top_Item := GR_VString (HAC_Float'Image (Top_Item.R));
      when SF_Image_Attribute_Bools  => Top_Item := GR_VString (Boolean'Image (Boolean'Val (Top_Item.I)));
      when SF_Image_Attribute_Chars  => Top_Item := GR_VString (Character'Image (Character'Val (Top_Item.I)));
      when SF_Image_Attribute_Durs   => Top_Item := GR_VString (Duration'Image (Top_Item.Dur));
      when SF_Image_Attribute_Enums  =>
        --  .Name contains the upper case representation as required by RM 3.5 (32).
        Top_Item := GR_VString (To_String (CD.IdTab (Natural (ND.IR.X) + Natural (Top_Item.I)).name));
      --
      when SF_Value_Attribute_Bools  =>
        begin
          Top_Item.I := Boolean'Pos (Boolean'Value (HAL.To_String (Top_Item.V)));
        exception
          when E : Constraint_Error =>
            Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E), True);
        end;
      when SF_Value_Attribute_Chars  =>
        begin
          Top_Item.I := Character'Pos (Character'Value (HAL.To_String (Top_Item.V)));
        exception
          when E : Constraint_Error =>
            Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E), True);
        end;
      when SF_Value_Attribute_Durs   =>
        begin
          Top_Item := GR_Duration (Duration'Value (HAL.To_String (Top_Item.V)));
        exception
          when E : Constraint_Error =>
            Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E), True);
        end;
      when SF_Value_Attribute_Enums  =>
        --  If there is a performance issue here, we could replace
        --  this linear search with something using Hashed_Maps.
        declare
          to_match_any_case : constant String := HAL.To_String (Top_Item.V);
          to_match : constant String := HAL.ACH.To_Upper (to_match_any_case);
          j : HAC_Integer := -1;
        begin
          for i in 0 .. CD.IdTab (Natural (ND.IR.X)).xtyp.Discrete_Last loop
            if To_String (CD.IdTab (Natural (ND.IR.X) + Natural (i + 1)).name) = to_match then
              j := i;
              exit;
            end if;
          end loop;
          if j >= 0 then
            Top_Item.I := j;
          else
            Raise_Standard (ND, VME_Constraint_Error,
              '"' & to_match_any_case &
              """ is not a literal of enumeration type """ &
              To_String (CD.IdTab (Natural (ND.IR.X)).name_with_case) & '"',
              True);
          end if;
        end;
      when SF_Get_Env =>
        declare
          Name : constant String := HAL.VStr_Pkg.To_String (Top_Item.V);
          use Ada.Environment_Variables;
        begin
          if Exists (Name) then
            Top_Item.V := HAL.To_VString (Value (Name));
          else
            Top_Item.V := HAL.Null_VString;
          end if;
        end;
      when SF_Directory_Exists | SF_Exists | SF_File_Exists =>
        declare
          Name : constant String := HAL.VStr_Pkg.To_String (Top_Item.V);
          use Ada.Directories;
        begin
          Top_Item.I := Boolean'Pos (
            Exists (Name) and then
              (Code = SF_Exists
               or else (Code = SF_Directory_Exists and then Kind (Name) = Directory)
               or else (Code = SF_File_Exists and then Kind (Name) = Ordinary_File)
              )
            );
        end;
      when SF_Niladic =>
        --  NILADIC functions need to push a new item (their own result).
        Push (ND);
        case SF_Niladic (Code) is
          when SF_Clock =>
            ND.S (Curr_TCB.T) := GR_Time (Ada.Calendar.Clock);
          when SF_Random_Float =>
            ND.S (Curr_TCB.T) := GR_Real (Defs.HAC_Float (Random (ND.Gen)));
          when SF_Null_VString =>
            ND.S (Curr_TCB.T) := GR_VString (HAL.Null_VString);
          when SF_Argument_Count | SF_Directory_Separator |
               SF_Current_Directory | SF_Get_Needs_Skip_Line |
               SF_Command_Name =>
            --  Those functions have been already processed at an
            --  upper calling level by Do_Standard_Function.
            null;
        end case;
      when SF_EOF | SF_EOLN | SF_Argument =>
        --  Those functions have been already processed at an
        --  upper calling level by Do_Standard_Function.
        null;
    end case;
  end Do_SF_Operator;

end HAC_Sys.PCode.Interpreter.Operators;
