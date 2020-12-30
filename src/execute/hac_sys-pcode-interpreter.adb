with HAC_Sys.PCode.Interpreter.Calls,
     HAC_Sys.PCode.Interpreter.Composite_Data,
     HAC_Sys.PCode.Interpreter.Exceptions,
     HAC_Sys.PCode.Interpreter.In_Defs,
     HAC_Sys.PCode.Interpreter.Multi_Statement,
     HAC_Sys.PCode.Interpreter.Operators,
     HAC_Sys.PCode.Interpreter.Tasking;

with HAC_Pack;

with Ada.Command_Line,
     Ada.Directories,
     Ada.Environment_Variables,
     Ada.Exceptions,
     Ada.IO_Exceptions;

package body HAC_Sys.PCode.Interpreter is

  procedure Interpret (CD : Compiler_Data; Unhandled : out Exception_Propagation_Data)
  is
    use In_Defs, Exceptions;
    ND : Interpreter_Data;

    procedure Pop (Amount : Positive := 1) is  begin Pop (ND, Amount); end Pop;
    procedure Push (Amount : Positive := 1) is begin Push (ND, Amount); end Push;

    procedure Start_Interpreter is
    begin
      ND.S := new Stack_Type;
      ND.PS := Running;
      ND.Start_Time := Ada.Calendar.Clock;
      ND.Snap     := False;
      ND.SWITCH   := False;           --  invoke scheduler on next cycle flag
      ND.SYSCLOCK := ND.Start_Time;
      ND.TIMER    := ND.SYSCLOCK;     --  set to end of current task's time slice
      HAC_Sys.PCode.Interpreter.Tasking.Init_main_task (CD, ND);
      HAC_Sys.PCode.Interpreter.Tasking.Init_other_tasks (CD, ND);
    end Start_Interpreter;

    procedure Do_Standard_Function is
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      Top_Item : General_Register renames ND.S (Curr_TCB.T);
      Code : constant SF_Code := SF_Code'Val (ND.IR.Y);
      use Defs;
    begin
      case Code is
        when SF_File_Information =>
          if ND.IR.X = 0 then  --  Niladic File info function -> abstract console
            Push;
            case SF_File_Information (Code) is
              when SF_EOF  => ND.S (Curr_TCB.T).I := Boolean'Pos (Console.End_Of_File);
              when SF_EOLN => ND.S (Curr_TCB.T).I := Boolean'Pos (Console.End_Of_Line);
            end case;
          else
            case SF_File_Information (Code) is
              when SF_EOF =>
                ND.S (Curr_TCB.T).I := Boolean'Pos (Ada.Text_IO.End_Of_File (ND.S (Curr_TCB.T).Txt.all));
              when SF_EOLN =>
                ND.S (Curr_TCB.T).I := Boolean'Pos (Ada.Text_IO.End_Of_Line (ND.S (Curr_TCB.T).Txt.all));
            end case;
          end if;
        when SF_Argument =>
          --  The stack top item may change its type here.
          declare
            arg_i : constant Integer := Integer (Top_Item.I);
          begin
            if arg_i not in 1 .. System_Calls.Argument_Count then
              Raise_Standard (ND, VME_Constraint_Error, "Argument number not in 1 .. Argument_Count");
              raise VM_Raised_Exception;
            end if;
            Top_Item := GR_VString (System_Calls.Argument (arg_i));
          end;
        when SF_Argument_Count =>
          Push;  --  Niladic function, needs to push a new item (their own result).
          ND.S (Curr_TCB.T).I := HAC_Integer (System_Calls.Argument_Count);
        when SF_Command_Name =>
          Push;  --  Niladic function, needs to push a new item (their own result).
          ND.S (Curr_TCB.T) := GR_VString (To_VString (System_Calls.Command_Name));
        when SF_Directory_Separator =>
          Push;  --  Niladic function, needs to push a new item (their own result).
          ND.S (Curr_TCB.T).I := Character'Pos (System_Calls.Directory_Separator);
        when SF_Current_Directory =>
          Push;  --  Niladic function, needs to push a new item (their own result).
          ND.S (Curr_TCB.T) := GR_VString (Ada.Directories.Current_Directory);
        when SF_Get_Needs_Skip_Line =>
          Push;  --  Niladic function, needs to push a new item (their own result).
          ND.S (Curr_TCB.T).I := Boolean'Pos (Console.Get_Needs_Skip_Line);
        when others =>
          Operators.Do_SF_Operator (CD, ND);  --  Doesn't need generic stuff.
      end case;
    end Do_Standard_Function;

    procedure Do_Text_Read (Code : SP_Code) is
      CH : Character;
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      use Defs;
      Out_Param : constant Index := Index (ND.S (Curr_TCB.T).I);
      Typ : Typen;
      Immediate : constant Boolean := Code = SP_Get_Immediate;
      FP : File_Ptr;
      String_Length_Decoding : Operand_2_Type;
      use type Operand_2_Type;
    begin
      Typ := Typen'Val (ND.IR.Y mod (2 ** Typen'Size));
      if Code in SP_Get .. SP_Get_Line then
        --  The End_Of_File_Console check is skipped here (disturbs GNAT's run-time).
        case Typ is
          when Ints     => Console.Get (ND.S (Out_Param).I);
          when Floats   =>
            ND.S (Out_Param) := GR_Real (0.0);  --  First, switch type to Floats.
            Console.Get (ND.S (Out_Param).R);
          when VStrings => ND.S (Out_Param) := GR_VString (Console.Get_Line);
          when Chars    =>
            if Immediate then
              Console.Get_Immediate (CH);
            else
              Console.Get (CH);
            end if;
            ND.S (Out_Param).I := Character'Pos (CH);
          when Arrays =>
            --  We have a String, with a given length.
            String_Length_Decoding := ND.IR.Y / (2 ** Typen'Size);
            for i in 1 .. Index (String_Length_Decoding) loop
              Console.Get (CH);
              ND.S (Out_Param + i - 1).I := Character'Pos (CH);
            end loop;
          when others =>
            null;
        end case;
        if Code = SP_Get_Line and Typ /= VStrings then
          Console.Skip_Line;
        end if;
        Pop;
      else
        FP := ND.S (Curr_TCB.T - 1).Txt;
        if Ada.Text_IO.End_Of_File (FP.all) then
          raise VM_End_Error;
        end if;
        case Typ is
          when Ints =>
            Defs.IIO.Get (FP.all, ND.S (Out_Param).I);
          when Floats =>
            Defs.RIO.Get (FP.all, ND.S (Out_Param).R);
          when Chars =>
            Ada.Text_IO.Get (FP.all, CH);
            ND.S (Out_Param).I := Character'Pos (CH);
          when VStrings =>
            ND.S (Out_Param) := GR_VString (Ada.Text_IO.Get_Line (FP.all));
          when Arrays =>
            --  We have a String, with a given length.
            String_Length_Decoding := ND.IR.Y / (2 ** Typen'Size);
            for i in 1 .. Index (String_Length_Decoding) loop
              --  A.10.7 (15): "Determines the length of the given string and
              --  attempts that number of Get operations for successive
              --  characters of the string (in particular, no operation
              --  is performed if the string is null)."
              if Ada.Text_IO.End_Of_File (FP.all) then
                raise VM_End_Error;
              end if;
              Ada.Text_IO.Get (FP.all, CH);
              ND.S (Out_Param + i - 1).I := Character'Pos (CH);
            end loop;
          when others =>
            null;
        end case;
        if Code = SP_Get_Line_F and Typ /= VStrings then
          Ada.Text_IO.Skip_Line (FP.all);
        end if;
        Pop (2);
      end if;
      ND.SWITCH := True;  --  give up control when doing I/O
    exception
      when E : Constraint_Error =>
        Raise_Standard (ND, VME_Constraint_Error, Ada.Exceptions.Exception_Message (E));
        raise VM_Raised_Exception;
      when E : Ada.IO_Exceptions.Data_Error =>
        Raise_Standard (ND, VME_Data_Error, Ada.Exceptions.Exception_Message (E));
        raise VM_Raised_Exception;
    end Do_Text_Read;

    procedure Do_Write_Formatted (Code : SP_Code) is
      Curr_TCB : Task_Control_Block renames   ND.TCB (ND.CurTask);
      FP       : File_Ptr;
      Item     : General_Register renames ND.S (Curr_TCB.T - 3);
      Format_1 : constant      Integer := Integer (ND.S (Curr_TCB.T - 2).I);
      Format_2 : constant      Integer := Integer (ND.S (Curr_TCB.T - 1).I);
      Format_3 : constant      Integer := Integer (ND.S (Curr_TCB.T).I);
      --  Valid parameters used: see def_param in HAC.Parser.Standard_Procedures.
      use Defs, Ada.Text_IO;
    begin
      if Code in SP_Put .. SP_Put_Line then
        case Typen'Val (ND.IR.Y) is
          when Ints            => Console.Put (Item.I, Field (Format_1), Number_Base (Format_2));
          when Floats          => Console.Put (Item.R, Field (Format_1), Field (Format_2), Field (Format_3));
          when Bools           => Console.Put (Boolean'Val (Item.I), Field (Format_1));
          when Chars           => Console.Put (Character'Val (Item.I));
          when VStrings        => Console.Put (To_String (Item.V));
          when String_Literals => Console.Put (
              CD.Strings_Constants_Table (Format_1 .. Format_1 + Integer (Item.I) - 1)
            );
          when Arrays          => Console.Put (Get_String_from_Stack (ND, Integer (Item.I), Format_1));
          when others =>
            null;
        end case;
        if Code = SP_Put_Line then
          Console.New_Line;
        end if;
        Pop (4);
      else
        FP := ND.S (Curr_TCB.T - 4).Txt;
        case Typen'Val (ND.IR.Y) is
          when Ints            => IIO.Put         (FP.all, Item.I, Field (Format_1), Number_Base (Format_2));
          when Floats          => RIO.Put         (FP.all, Item.R, Field (Format_1), Field (Format_2), Field (Format_3));
          when Bools           => BIO.Put         (FP.all, Boolean'Val (Item.I), Field (Format_1));
          when Chars           => Ada.Text_IO.Put (FP.all, Character'Val (Item.I));
          when VStrings        => Ada.Text_IO.Put (FP.all, To_String (Item.V));
          when String_Literals => Ada.Text_IO.Put (FP.all,
              CD.Strings_Constants_Table (Format_1 .. Format_1 + Integer (Item.I) - 1)
            );
          when Arrays          => Ada.Text_IO.Put (FP.all,
              Get_String_from_Stack (ND, Integer (Item.I), Format_1));
          when others =>
            null;
        end case;
        if Code = SP_Put_Line_F then
          Ada.Text_IO.New_Line (FP.all);
        end if;
        Pop (5);
      end if;
      ND.SWITCH := True;  --  give up control when doing I/O
    end Do_Write_Formatted;

    procedure Do_Code_for_Automatic_Initialization is
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      use Defs;
      Var_Addr : constant Index := Index (ND.S (Curr_TCB.T).I);
    begin
      case Typen'Val (ND.IR.Y) is
        when VStrings   => ND.S (Var_Addr) := GR_VString (Null_VString);
        when Text_Files => Allocate_Text_File (ND, ND.S (Var_Addr));
        when others     => null;
      end case;
      Pop;
    end Do_Code_for_Automatic_Initialization;

    procedure Do_Update_Display_Vector is
      --  Emitted at the end of Subprogram_or_Entry_Call, when the
      --  called subprogram's nesting level is *lower* than the caller's.
      Low_Level  : constant Nesting_level := Nesting_level (ND.IR.X);  --  Called.
      High_Level : constant Nesting_level := Nesting_level (ND.IR.Y);  --  Caller.
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      H3 : Defs.Index;
    begin
      H3 := Curr_TCB.B;
      for L in reverse Low_Level + 1 .. High_Level loop
        Curr_TCB.DISPLAY (L) := H3;
        H3 := Defs.Index (ND.S (H3 + 2).I);
      end loop;
    end Do_Update_Display_Vector;

    procedure Do_File_IO is
      Code : constant SP_Code := SP_Code'Val (ND.IR.X);
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      use Defs.VStrings_Pkg;
      Lines : Ada.Text_IO.Positive_Count;
    begin
      case Code is
        when SP_Open =>
          Pop (2);
          Ada.Text_IO.Open (
            ND.S (Curr_TCB.T + 1).Txt.all,
            Ada.Text_IO.In_File,
            To_String (ND.S (Curr_TCB.T + 2).V)
          );
        when SP_Append =>
          Pop (2);
          Ada.Text_IO.Open (
            ND.S (Curr_TCB.T + 1).Txt.all,
            Ada.Text_IO.Append_File,
            To_String (ND.S (Curr_TCB.T + 2).V)
          );
        when SP_Create =>
          Pop (2);
          Ada.Text_IO.Create (
            ND.S (Curr_TCB.T + 1).Txt.all,
            Ada.Text_IO.Out_File,
            To_String (ND.S (Curr_TCB.T + 2).V)
          );
        when SP_Close =>
          Ada.Text_IO.Close (ND.S (Curr_TCB.T).Txt.all);
          Pop;
        when SP_Set_Env =>
          Ada.Environment_Variables.Set (
            To_String (ND.S (Curr_TCB.T - 1).V),
            To_String (ND.S (Curr_TCB.T).V)
          );
          Pop (2);
        when SP_Copy_File =>
          Ada.Directories.Copy_File (
            To_String (ND.S (Curr_TCB.T - 1).V),
            To_String (ND.S (Curr_TCB.T).V)
          );
          Pop (2);
        when SP_Delete_File =>
          Ada.Directories.Delete_File (To_String (ND.S (Curr_TCB.T).V));
          Pop;
        when SP_Rename =>
          Ada.Directories.Rename (
            To_String (ND.S (Curr_TCB.T - 1).V),
            To_String (ND.S (Curr_TCB.T).V)
          );
          Pop (2);
        when SP_Set_Directory =>
          Ada.Directories.Set_Directory (To_String (ND.S (Curr_TCB.T).V));
          Pop;
        when SP_Push_Abstract_Console =>
          Push;
          ND.S (Curr_TCB.T) := GR_Abstract_Console;
        when SP_Get | SP_Get_Immediate | SP_Get_Line | SP_Get_F | SP_Get_Line_F =>
          Do_Text_Read (Code);
        when SP_Put | SP_Put_Line | SP_Put_F | SP_Put_Line_F =>
          Do_Write_Formatted (Code);
        when SP_New_Line =>
          Lines := Ada.Text_IO.Positive_Count (ND.S (Curr_TCB.T).I);
          if ND.S (Curr_TCB.T - 1).Txt = Abstract_Console then
            Console.New_Line (Lines);
          else
            Ada.Text_IO.New_Line (ND.S (Curr_TCB.T - 1).Txt.all, Lines);
          end if;
          Pop (2);
        when SP_Skip_Line =>
          Lines := Ada.Text_IO.Positive_Count (ND.S (Curr_TCB.T).I);
          if ND.S (Curr_TCB.T - 1).Txt = Abstract_Console then
            --  The End_Of_File_Console check is skipped here (disturbs GNAT's run-time).
            Console.Skip_Line (Lines);
          elsif Ada.Text_IO.End_Of_File (ND.S (Curr_TCB.T - 1).Txt.all) then
            raise VM_End_Error;
          else
            Ada.Text_IO.Skip_Line (ND.S (Curr_TCB.T - 1).Txt.all, Lines);
          end if;
          Pop (2);
        when SP_Shell_Execute_with_Result =>
          declare
            Command        : constant String      := To_String (ND.S (Curr_TCB.T - 1).V);
            Result         :          Integer;
            Result_Address : constant Defs.Index  := Defs.Index (ND.S (Curr_TCB.T).I);
          begin
            System_Calls.Shell_Execute (Command, Result);
            ND.S (Result_Address).I := Defs.HAC_Integer (Result);
            Pop (2);
          end;
        when SP_Shell_Execute_without_Result =>
          declare
            Command : constant String := To_String (ND.S (Curr_TCB.T).V);
            Dummy   : Integer;
          begin
            System_Calls.Shell_Execute (Command, Dummy);
            Pop;
          end;
        when SP_Set_Exit_Status =>
          HAC_Pack.Set_Exit_Status (Integer (ND.S (Curr_TCB.T).I));
          Pop;
        when SP_Wait | SP_Signal | SP_Priority | SP_InheritP | SP_Quantum =>
          null;
      end case;
      ND.SWITCH := True;  --  give up control when doing I/O
    exception
      when Ada.Text_IO.Name_Error =>
        case Code is
          when SP_Open | SP_Create | SP_Append =>
            Raise_Standard (ND, VME_Name_Error,
              "File not found, or invalid name: " & To_String (ND.S (Curr_TCB.T + 2).V));
          when others =>
            Raise_Standard (ND, VME_Name_Error);
        end case;
        raise VM_Raised_Exception;
      when E : Ada.Text_IO.Status_Error =>
        case Code is
          when SP_Open | SP_Create | SP_Append =>
            Raise_Standard (ND, VME_Status_Error, "File already open");
          when SP_Close =>
            Raise_Standard (ND, VME_Status_Error, "File not open");
          when others =>
            Raise_Standard (ND, VME_Status_Error, "File not open? [" &
              Ada.Exceptions.Exception_Message (E) & ']');
        end case;
      when Ada.Text_IO.Use_Error =>
        case Code is
          when SP_Open | SP_Create | SP_Append =>
            Raise_Standard (ND, VME_Use_Error,
              "Cannot access file: " & To_String (ND.S (Curr_TCB.T + 2).V));
          when others =>
            Raise_Standard (ND, VME_Use_Error);
        end case;
        raise VM_Raised_Exception;
    end Do_File_IO;

    procedure Add_Stack_Trace_Line (Offset : Natural) is
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      D : Debug_Info renames CD.ObjCode (Curr_TCB.PC - Offset).D;
      use Defs.VStrings_Pkg;
    begin
      if D.Full_Block_Id /= Universe then
        ND.TCB (ND.CurTask).Exception_Info.ST_Message.Append (D);
      end if;
    end Add_Stack_Trace_Line;

    procedure Fetch_Instruction is
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
    begin
      if ND.PS = Exception_Raised then
        while not OK_for_Exception (CD.ObjCode (Curr_TCB.PC).F) loop
          Curr_TCB.PC := Curr_TCB.PC + 1;
        end loop;
      end if;
      ND.IR := CD.ObjCode (Curr_TCB.PC);
      Curr_TCB.PC := Curr_TCB.PC + 1;
    end Fetch_Instruction;

    procedure Execute_Current_Instruction is
      Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
      IR : Order renames ND.IR;
      use Defs;
      use type HAC_Integer;
      --
      procedure Do_Atomic_Data_Push_Operation is
      begin
        Push;
        case Atomic_Data_Push_Opcode (ND.IR.F) is
          when k_Push_Address =>           --  Push "v'Access" of variable v
            ND.S (Curr_TCB.T).I := HAC_Integer (Curr_TCB.DISPLAY (Nesting_level (IR.X))) + IR.Y;
          when k_Push_Value =>             --  Push variable v's value.
            ND.S (Curr_TCB.T) := ND.S (Curr_TCB.DISPLAY (Nesting_level (IR.X)) + Index (IR.Y));
          when k_Push_Indirect_Value =>    --  Push "v.all" (v is an access).
            ND.S (Curr_TCB.T) := ND.S (Index (ND.S (Curr_TCB.DISPLAY (Nesting_level (IR.X)) + Index (IR.Y)).I));
          when k_Push_Discrete_Literal =>  --  Literal: discrete value (Integer, Character, Boolean, Enum)
            ND.S (Curr_TCB.T).I := IR.Y;
          when k_Push_Float_Literal =>
            ND.S (Curr_TCB.T) := (
              Special => Defs.Floats,
              I       => 0,
              R       => CD.Float_Constants_Table (Integer (IR.Y))
            );
        end case;
      end Do_Atomic_Data_Push_Operation;
      --
      procedure Do_Swap is
        temp : constant General_Register := ND.S (Curr_TCB.T);
      begin
        ND.S (Curr_TCB.T)     := ND.S (Curr_TCB.T - 1);
        ND.S (Curr_TCB.T - 1) := temp;
      end Do_Swap;
      --
    begin
      case ND.IR.F is
        when k_Jump => Curr_TCB.PC := Index (IR.Y);
        when k_Conditional_Jump =>
          if ND.S (Curr_TCB.T).I = 0 then  --  if False, then ...
            Curr_TCB.PC := Index (IR.Y);   --  ... Jump.
          end if;
          Pop;
        when k_Store =>  --  [T-1].all := [T]
          ND.S (Index (ND.S (Curr_TCB.T - 1).I)) := ND.S (Curr_TCB.T);
          Pop (2);
        when k_Swap => Do_Swap;
        --
        when k_Variable_Initialization => Do_Code_for_Automatic_Initialization;
        when k_Update_Display_Vector   => Do_Update_Display_Vector;
        when k_File_I_O                => Do_File_IO;
        when k_Standard_Functions      => Do_Standard_Function;
        --
        when Multi_Statement_Opcode  => Multi_Statement.Do_Multi_Statement_Operation (CD, ND);
        when Atomic_Data_Push_Opcode => Do_Atomic_Data_Push_Operation;
        when Composite_Data_Opcode   => Composite_Data.Do_Composite_Data_Operation (CD, ND);
        when Unary_Operator_Opcode   => Operators.Do_Unary_Operator (ND);
        when Binary_Operator_Opcode  => Operators.Do_Binary_Operator (ND);
        when Calling_Opcode          => Calls.Do_Calling_Operation (CD, ND);
        when Tasking_Opcode          => Tasking.Do_Tasking_Operation (CD, ND);
      end case;
    exception
      when others =>
        Add_Stack_Trace_Line (Offset => 0);
        ND.PS := Exception_Raised;
        raise;
    end Execute_Current_Instruction;

    procedure Execute_Current_Instruction_with_Exception is
    begin
      Execute_Current_Instruction;
      if ND.PS = Exception_Raised then
        --  We have just executed an Exit, so the last instruction (with
        --  the program counter back to the caller side), was a Call.
        Add_Stack_Trace_Line (Offset => 1);
      end if;
    exception
      when VM_Case_Check_Error =>
        Raise_Standard (ND, VME_Program_Error, "CASE Statement doesn't cover all cases");
      when VM_Division_by_0 =>
        Raise_Standard (ND, VME_Constraint_Error, "Division by 0");
      when VM_End_Error =>
        Raise_Standard (ND, VME_End_Error, "");
      when VM_Function_End_without_Return =>
        Raise_Standard (ND, VME_Program_Error, "Function's end reached without ""return"" statement");
      when VM_Out_of_Range  =>
        Raise_Standard (ND, VME_Constraint_Error, "Out of range");
      when VM_Stack_Overflow  =>
        Raise_Standard (ND, VME_Storage_Error, "Stack overflow");
      when VM_Stack_Underflow =>
        Raise_Standard (ND, VME_Storage_Error, "Stack underflow");
      when VM_Raised_Exception =>
        null;  --  HAC exception has been already raised (see Name_Error for an example).
    end Execute_Current_Instruction_with_Exception;

    procedure Single_Task_Delays is
      User_Aborted : Boolean;
      use Ada.Calendar, Tasking;
      wake : constant Time := ND.TCB (ND.CurTask).WAKETIME;
    begin
      ND.SYSCLOCK := Clock;
      if wake > ND.SYSCLOCK then
        while wake - ND.SYSCLOCK > TSlice loop
          Feedback (
            Stack_Current => ND.TCB (ND.CurTask).T,
            Stack_Total   => ND.S'Last,
            Wall_Clock    => ND.SYSCLOCK,
            User_Abort    => User_Aborted
          );
          if User_Aborted then
            Raise_Standard (ND, VME_User_Abort, "");
          end if;
          delay TSlice;
          ND.SYSCLOCK := Clock;
        end loop;
        delay Duration'Max (0.0, wake - ND.SYSCLOCK);
      end if;
      ND.SWITCH := False;
    end Single_Task_Delays;

    User_Aborted : Boolean;

  begin  --  Interpret
    Start_Interpreter;
    --
    Running_State :
    loop  --  until Processor state is not Running or Exception_Raised
      if ND.Scheduler = Single_Task then
        Single_Task_Delays;
      else
        Tasking.Scheduling (CD, ND);
      end if;
      Feedback (
        Stack_Current => ND.TCB (ND.CurTask).T,
        Stack_Total   => ND.S'Last,
        Wall_Clock    => ND.SYSCLOCK,
        User_Abort    => User_Aborted
      );
      if User_Aborted then
        Raise_Standard (ND, VME_User_Abort, "");
      end if;
      exit Running_State when ND.PS = DEADLOCK or ND.PS = FIN;
      --
      Fetch_Instruction;
      --  HERE IS THE POINT WHERE THE TASK MONITORING IS CALLED (removed)
      Execute_Current_Instruction_with_Exception;
      --
      exit Running_State when ND.PS not in Running_or_in_Exception;
    end loop Running_State;
    --
    if ND.PS not in Exception_Raised .. FIN then
      Post_Mortem_Dump (CD, ND);
    end if;
    --
    Free_Allocated_Contents (ND);
    --
    Unhandled := ND.TCB (ND.CurTask).Exception_Info;
    --  Use Is_Exception_Raised to check whether an exception was
    --  unhandled when leaving the interpreter.
    case ND.PS is
      when FIN              => null;  --  All good, end reached (can have an unhandled exception).
      when Running          => null;  --  Should not happen here.
      when Exception_Raised => null;  --  Unhandled exception information stored in Unhandled.
      when DEADLOCK         => raise Abnormal_Termination with "Tasking: Deadlock";
      when WAIT             => raise Abnormal_Termination with "Tasking: Wait";
    end case;
  end Interpret;

  function Current_IO_Get_Needs_Skip_Line return Boolean is
  begin
    return True;  --  The input is buffered with Ada.Text_IO.Get (not Get_Immediate).
  end Current_IO_Get_Needs_Skip_Line;

  procedure Interpret_on_Current_IO (
    CD_CIO           :     Compiler_Data;
    Argument_Shift   :     Natural := 0;    --  Number of arguments to be skipped
    Full_Script_Name :     String;
    Unhandled        : out Exception_Propagation_Data;
    Max_Stack_Usage  : out Natural;
    Stack_Size       : out Positive
  )
  is

    procedure No_Feedback (
      Stack_Current, Stack_Total : in     Natural;
      Wall_Clock                 : in     Ada.Calendar.Time;
      User_Abort                 :    out Boolean
    )
    is
    pragma Unreferenced (Stack_Total, Wall_Clock);
    begin
      Max_Stack_Usage := Integer'Max (Max_Stack_Usage, Stack_Current);
      User_Abort := False;  --  Ctrl-C will make it - in a less polite way...
    end No_Feedback;

    function Shifted_Argument_Count return Natural is
    begin
      return Ada.Command_Line.Argument_Count - Argument_Shift;
    end Shifted_Argument_Count;

    function Shifted_Argument (Number : Positive) return String is
    begin
      return Ada.Command_Line.Argument (Number + Argument_Shift);
    end Shifted_Argument;

    function Custom_Command_Name return String is
    begin
      return Full_Script_Name;
    end Custom_Command_Name;

    package Current_IO_Console is new
      Console_Traits
         (Ada.Text_IO.End_Of_File,
          Ada.Text_IO.End_Of_Line,
          Current_IO_Get_Needs_Skip_Line,
          Defs.IIO.Get,
          Defs.RIO.Get,
          Ada.Text_IO.Get,
          Ada.Text_IO.Get_Immediate,
          Ada.Text_IO.Get_Line,
          Ada.Text_IO.Skip_Line,
          Defs.IIO.Put,
          Defs.RIO.Put,
          Defs.BIO.Put,
          Ada.Text_IO.Put,
          Ada.Text_IO.Put,
          Ada.Text_IO.New_Line
         );

    package Custom_System_Calls is new
      System_Calls_Traits
         (Shifted_Argument_Count,
          Shifted_Argument,
          Custom_Command_Name,
          HAC_Pack.Shell_Execute,
          HAC_Pack.Directory_Separator
         );

    procedure Interpret_on_Current_IO_Instance is new
      Interpret
         (No_Feedback,
          Current_IO_Console,
          Custom_System_Calls
         );

  begin
    Stack_Size := In_Defs.Stack_Type'Last;
    Max_Stack_Usage := 0;
    Interpret_on_Current_IO_Instance (CD_CIO, Unhandled);
  end Interpret_on_Current_IO;

  function Image (E : Exception_Propagation_Data) return String is
  begin
    case E.Currently_Raised.Ex_Typ is
      when No_Exception         => return "";
      when VME_Constraint_Error => return "Constraint_Error";
      when VME_Data_Error       => return "Data_Error";
      when VME_End_Error        => return "End_Error";
      when VME_Name_Error       => return "Name_Error";
      when VME_Program_Error    => return "Program_Error";
      when VME_Status_Error     => return "Status_Error";
      when VME_Storage_Error    => return "Storage_Error";
      when VME_Use_Error        => return "Use_Error";
      when VME_User_Abort       => return "User_Abort";
      when VME_Custom           => return "(custom)";  --  needs to use details
    end case;
  end Image;

  function Message (E : Exception_Propagation_Data) return String is
  begin
    return Defs.To_String (E.Exception_Message);
  end Message;

  function Is_Exception_Raised (E : Exception_Propagation_Data) return Boolean is
  begin
    return E.Currently_Raised.Ex_Typ /= No_Exception;
  end Is_Exception_Raised;

  procedure Show_Trace_Back (E : Exception_Propagation_Data) is
  begin
    for STL of E.ST_Message loop
      Show_Line_Information (
        Defs.To_String (STL.File_Name),
        Defs.To_String (STL.Full_Block_Id),
        STL.Line_Number
      );
    end loop;
  end Show_Trace_Back;

end HAC_Sys.PCode.Interpreter;
