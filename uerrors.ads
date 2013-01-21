Package UErrors is

  PROCEDURE Error(N: Integer);
	
  PROCEDURE EndSkip;
	
  PROCEDURE ErrorMsg;
	
  PROCEDURE Fatal(N: Integer); -- internal table overflow

  Failure_1_0: exception;

  FUNCTION ErrorString(Id: Integer) return String; -- for debug

end UErrors;
