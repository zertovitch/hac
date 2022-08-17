--  Files f and g are left open (not closed) during the variables lifetime.
--  `hac -v1 open_files.adb` will show their names.

with HAT;

procedure Open_Files is
  use HAT;
  procedure Sub is
    g : File_Type;
  begin
    Open (g, "floats.adb");
  end;
  f : File_Type;
begin
  Sub;
  Open (f, "open_files.adb");
end Open_Files;

