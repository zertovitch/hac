with HAL;

package Testing_Utilities is

  procedure Failure (Message : HAL.VString);

  procedure Assert (Check : in Boolean; Message : HAL.VString);
  --  Similar to pragma Assert (RM 11.4.2) but sets an exit status
  --  instead of raising an exception.

end Testing_Utilities;
