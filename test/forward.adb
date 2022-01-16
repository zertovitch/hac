--  Test forward subprogram declaration
--  https://en.wikipedia.org/wiki/Forward_declaration

--  Code taken from:
--  http://rosettacode.org/wiki/Mutual_recursion#Ada
--
--  Hofstadter Female and Male sequences
--
--  F :  1 1 2 2 3 3 4 5 5 6 6 7 8 8 9 9 10 11 11 12 13 13 14 14
--  M :  0 0 1 2 2 3 4 4 5 6 6 7 7 8 9 9 10 11 11 12 12 13 14 14
--
--  N :                      1 1 1
--       0 1 2 3 4 5 6 7 8 9 0 1 2
--
with HAL;

procedure Forward is

  use HAL;

  procedure Failure (Msg : VString) is
  begin
    Put_Line (+"Failure in test: [" & Msg & ']');
    Set_Exit_Status (1);  --  Compiler test failed.
  end Failure;

  procedure Assert (Msg : VString; Check : in Boolean) is
    --  Similar to RM 11.4.2 but without raising an exception.
  begin
    if not Check then Failure (Msg & ", assertion"); end if;
  end Assert;

  function M (N : Integer) return Integer;

  function F (N : Integer) return Integer is
  begin
    if N = 0 then
      return 1;
    else
      return N - M (F (N - 1));
    end if;
  end F;

  function M (N : Integer) return Integer is
  begin
    if N = 0 then
      return 0;
    else
      return N - F (M (N - 1));
    end if;
  end M;

begin
  Assert (+"F", 4 = F (6));
  Assert (+"M", 4 = M (6));
  Assert (+"F", 8 = F (12));
  Assert (+"M", 7 = M (12));
end Forward;
