--  This is a minimalistic version of HAX, for showing the minimum code required
--  to run HAC on a given Ada source file and display outputs on Standard_Output.
--
--  See HAX for the full version of the command-line tool.

with HAC.Compiler, HAC.Co_Defs, HAC.PCode.Interpreter;
with Ada.Command_Line, Ada.Streams.Stream_IO, Ada.Text_IO;

procedure HAX_Mini is

  use Ada.Text_IO;

  procedure Compile_and_interpret_file (Ada_file_name : String) is
    use HAC.Compiler, HAC.Co_Defs, HAC.PCode.Interpreter;
    use Ada.Streams.Stream_IO;
    --
    f : Ada.Streams.Stream_IO.File_Type;
    CD : Compiler_Data;
    unhandled : Exception_Propagation_Data;
  begin
    Open (f, In_File, Ada_file_name);
    Set_Source_Stream (CD, Stream (f), Ada_file_name);
    Compile (CD);
    Close (f);
    --
    if Unit_Compilation_Successful (CD) then
      Interpret_on_Current_IO (CD, 1, Ada_file_name, unhandled);
      if Is_Exception_Raised (unhandled) then
        Put_Line (Current_Error, "HAC VM: raised " & Image (unhandled));
        Put_Line (Current_Error, Message (unhandled));
      end if;
    end if;
  end Compile_and_interpret_file;

  use Ada.Command_Line;

begin
  if Argument_Count = 0 then
    Put_Line (Current_Error, "Usage: hax_mini main.adb");
  else
    Compile_and_interpret_file (Argument (1));
  end if;
end HAX_Mini;
