with Ada.Text_IO;

procedure Show_MIT_License
  (file : Ada.Text_IO.File_Type; source_with_license : String)
is
  use Ada.Text_IO;
begin
  New_Line (file);
  Put_Line (file, "| This software is free and open-source.");
  Put_Line (file, "| It is provided ""as is"", WITHOUT WARRANTY OF ANY KIND.");
  Put_Line (file, "| For the full license wording, see the header" &
                  " (copyright & MIT license)");
  Put_Line (file, "| appearing on top of this software's source files.");
  Put_Line (file, "| In doubt, check the file: " & source_with_license);
  New_Line (file);
end Show_MIT_License;
