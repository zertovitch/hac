with HAT; use HAT;

procedure Existence is
  procedure Check (Name : VString) is
    procedure Tell (Existing : Boolean; As : VString) is
    begin
      Put (Name);
      if Existing then
        Put (" exists ");
      else
        Put (" does not exist ");
      end if;
      Put_Line (+"as " & As);
    end Tell;
  begin
    Tell (Exists (Name), +"anything in the file system");
    Tell (Directory_Exists (Name), +"a directory");
    Tell (File_Exists (Name), +"a file");
  end Check;
begin
  Check (+"existence.adb");
  Check (+"aoc");
end Existence;
