package body Testing_Utilities is

  use HAT;

  function OK_Code return Integer is
  begin
    return 0;  --  0 is the default exit code.
  end OK_Code;

  procedure Failure (Message : VString) is
  begin
    Put_Line (+"   ----> Failure in test: [" & Message & ']');
    Set_Exit_Status (1);  --  Test failed.
  end Failure;

  procedure Assert (Check : in Boolean; Message : VString) is
  begin
    if not Check then
      Failure (Message & ", assertion");
    end if;
  end Assert;

end Testing_Utilities;
