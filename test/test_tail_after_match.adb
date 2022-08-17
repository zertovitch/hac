with HAT;

procedure Test_Tail_After_Match is
  use HAT;
  Path : VString := +"/etc/genesix/gnx-startup";
begin
  Put_Line (Tail_After_Match (Path,  '/'));                         --  returns "gnx-startup"
  Put_Line (Tail_After_Match (Path,  "ix"));                        --  returns "/gnx-startup"
  Put_Line (Tail_After_Match (Path,  "gene"));                      --  returns "six/gnx-startup"
  Put_Line (Tail_After_Match (Path,  "etc/genesix/gnx-startu"));    --  returns "p"
  Put_Line (Tail_After_Match (Path,  "/etc/genesix/gnx-startu"));   --  returns "p" 
  Put_Line (Tail_After_Match (Path,  "/etc/genesix/gnx-startup"));  --  returns empty string 
  Put_Line (Tail_After_Match (Path, +"/etc/genesix/gnx-startupp")); --  returns empty string 
  Put_Line (Tail_After_Match (Path, +"/etc/geneseven"));            --  returns empty string 
end Test_Tail_After_Match;
