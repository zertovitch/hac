--  System's Environment Variables: Set & Get.

with HAT; use HAT;

procedure Env is
  path, path_v : VString;
begin
  path   := Get_Env ("PATH");   --  String argument
  path_v := Get_Env (+"PATH");  --  VString argument
  --
  Put_Line ("The PATH (to happiness) is : " & path);
  if path /= path_v then
    Put_Line ("Uh ?");
  end if;
  Set_Env ("HAC_Rules", "Good Day, Ladies and Gentlemen!");
  New_Line;
  Put_Line (
    "Important message from the environment variables: " &
    Get_Env ("HAC_Rules")
  );
end Env;
