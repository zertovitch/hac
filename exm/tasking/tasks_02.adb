--  Was originally Test2 in SmallAda

with HAT;

procedure Tasks_02 is

  compiler_regression_test_mode : constant Boolean := HAT.Argument_Count > 0;
  verbose : constant Boolean := not compiler_regression_test_mode;

  procedure Selective_Put_Line (M : HAT.VString) is
  begin
    if verbose then
      HAT.Put_Line (M);
    end if;
  end Selective_Put_Line;

  task T1;
  task T2 is
    entry Hereza_Num (V1 : Integer);
    entry Gimmea_Num (V2 : out Integer);
  end T2;

  the_answer : constant := 42;

  use HAT;

  task body T1 is
    iii : Integer;
  begin
    Selective_Put_Line (5 * "Task T1 starting... ");
    iii := the_answer;
    Selective_Put_Line (+"iii = " & iii);
    T2.Hereza_Num (iii);
  end T1;

  task body T2 is
    jjj : Integer;
  begin
    Selective_Put_Line (5 * "Task T2 starting... ");
    jjj := 0;
    Selective_Put_Line (+"jjj = " & jjj);
    accept Hereza_Num (V1 : Integer) do
      jjj := V1;
    end Hereza_Num;
    Selective_Put_Line (+"jjj = " & jjj);
    accept Gimmea_Num (V2 : out Integer) do
      V2 := jjj;
    end Gimmea_Num;
  end T2;

  Mmm : Integer := 99;

begin
  Selective_Put_Line (+"[Main] ---------- Test with Tasks.");
  Selective_Put_Line (+"[Main] [point 1] mmm = " & Mmm);
  T2.Gimmea_Num (Mmm);
  Selective_Put_Line (+"[Main] ---------- At this point, tasks are done.");
  Selective_Put_Line (+"[Main] [point 2] mmm = " & Mmm);
  Selective_Put_Line (+"[Main] Done.");
  if compiler_regression_test_mode and then Mmm /= the_answer then
    Put_Line ("   ----> Compiler test failed.");
    Set_Exit_Status (1);
  end if;
end Tasks_02;
