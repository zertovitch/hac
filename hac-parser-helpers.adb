-------------------------------------------------------------------------------------
--
--  HAC - HAC Ada Compiler
--
--  A compiler in Ada for an Ada subset
--
--  Copyright, license, etc. : see top package.
--
-------------------------------------------------------------------------------------
--

with HAC.Scanner;

package body HAC.Parser.Helpers is

  use HAC.Scanner;

  procedure Need (
    S       : KeyWSymbol;
    E       : Error_code;
    Forgive : KeyWSymbol := Dummy_Symbol
  )
  is
  begin
    if Sy = S then
      InSymbol;
    else
      Error (E);
      if Sy = Forgive then
        InSymbol;
      end if;
    end if;
  end Need;

  procedure Skip (FSys : Symset; N : Error_code) is

    function StopMe return Boolean is
    begin
      return False;
    end StopMe;

  begin
    Error (N);
    --
    SkipFlag := True;
    while not FSys (Sy) loop
      InSymbol;
      if StopMe then
        raise Failure_1_0;
      end if;
    end loop;

    InSymbol;    -- Manuel:  If this InSymbol call is
    -- omitted, the system will get in an
    -- infinite loop on the statement:
    --  put_lin("Typo is on purpose");

    if StopMe then
      raise Failure_1_0;
    end if;
    if SkipFlag then
      EndSkip;
    end if;
  end Skip;

  procedure Skip (S : KeyWSymbol; N : Error_code) is
  begin
    Skip (Singleton (S), N);
  end Skip;

  procedure Test (
    S1, S2        : Symset;
    N             : Error_code;
    stop_on_error : Boolean:= False)
  is
  begin
    if not S1 (Sy) then
      if stop_on_error then
        Error (N, stop_on_error => True);
      end if;
      Skip (S1 + S2, N);
    end if;
  end Test;

  After_semicolon : constant Symset :=
    (IDent | TYPE_Symbol | TASK_Symbol => True, others => False) +
    Block_Begin_Symbol;

  Comma_or_colon : constant Symset :=
    Symset'(Comma | Colon => True, others => False);

  procedure Test_Semicolon (FSys : Symset) is
  begin
    if Sy = Semicolon then
      InSymbol;
    else
      Error (err_SEMICOLON_missing);
      if Comma_or_colon (Sy) then
        InSymbol;
      end if;
    end if;
    Test (After_semicolon, FSys, err_incorrectly_used_symbol);
  end Test_Semicolon;

  procedure Test_END_Symbol is
  begin
    if Sy = END_Symbol then
      InSymbol;
    else
      Skip (Semicolon, err_END_missing);
    end if;
  end Test_END_Symbol;

end HAC.Parser.Helpers;
