package body HAC.PCode.Interpreter.In_Defs is

  procedure Allocate_Text_File (
    ND : in out Interpreter_Data;
    R  : in out GRegister
  )
  is
  begin
    if R.Txt = null then
      R.Txt := new Ada.Text_IO.File_Type;
      ND.Files.Append (R.Txt);
    end if;
  end Allocate_Text_File;

  procedure Free_Allocated_Contents (
    ND : in out Interpreter_Data
  )
  is
    procedure Free is new Ada.Unchecked_Deallocation (Ada.Text_IO.File_Type, File_Ptr);
  begin
    for F of ND.Files loop
      Free (F);
    end loop;
  end Free_Allocated_Contents;

  procedure Pop (ND : in out Interpreter_Data; Amount : Positive := 1) is
    Curr_TCB_Top : Integer renames ND.TCB (ND.CurTask).T;
  begin
    Curr_TCB_Top := Curr_TCB_Top - Amount;
    if Curr_TCB_Top < ND.S'First then
      raise Stack_Underflow;
    end if;
  end Pop;

  procedure Push (ND : in out Interpreter_Data; Amount : Positive := 1) is
    Curr_TCB : Task_Control_Block renames ND.TCB (ND.CurTask);
  begin
    Curr_TCB.T := Curr_TCB.T + Amount;
    if Curr_TCB.T > Curr_TCB.STACKSIZE then
      raise Stack_Overflow;
    end if;
  end Push;

end HAC.PCode.Interpreter.In_Defs;
