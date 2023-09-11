--  Summary of what can be done with
--  compilation for native targets.
--  Currently (v.0.26): not much!
--
--  Command sample:
--  hac -tamd64_windows_console_fasm exm/native.adb

with HAT;

procedure Native is
  use HAT;
  --  a : Integer;
begin
  --  a := 1;  --  Variables: TBD.
  Put_Line ("Hello ...");
  Put_Line ("... world!");
  --  12345 in various forms:
  Put_Line (12000 + 340 + 5 * 1);
  Put_Line (12355 * (13 - 12) - 20 / (1414 / 707));
  Put_Line (2469 * 5);
  Put_Line (61725 / 5);
end Native;
