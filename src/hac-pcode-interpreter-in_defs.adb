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

end HAC.PCode.Interpreter.In_Defs;
