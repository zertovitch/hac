--  Was originally Test2 in SmallAda

with HAL;

procedure Tasks_02 is

  test_str : constant HAL.VString := HAL.Get_Env ("compiler_test_value_1");
  compiler_regression_test_mode : constant Boolean := test_str /= "";
  verbose : constant Boolean := not compiler_regression_test_mode;

  procedure Selective_Put_Line (M : HAL.VString) is
  begin
    if verbose then
      HAL.Put_Line (M);
    end if;
  end;

  task T1;
  task T2 is
    entry Hereza_Num (V1 : Integer);
    entry Gimmea_Num (V2 : out Integer);
  end T2;

  the_answer : constant := 42;

  task body T1 is
    Iii : Integer;
  begin
    Selective_Put_Line (5 * "Task T1 .. ");
    Iii := the_answer;
    Selective_Put_Line (+"iii = " & Iii);
    T2.Hereza_Num (Iii);
  end T1;

  task body T2 is
    Jjj : Integer;
  begin
    Selective_Put_Line (5 * "Task T2 .. ");
    Jjj := 0;
    Selective_Put_Line (+"jjj = " & Jjj);
    accept Hereza_Num (V1 : Integer) do
      Jjj := V1;
    end Hereza_Num;
    Selective_Put_Line (+"jjj = " & Jjj);
    accept Gimmea_Num (V2 : out Integer) do
      V2 := Jjj;
    end Gimmea_Num;
  end T2;

  Mmm : Integer := 99;

begin
  Selective_Put_Line (+"Test with Tasks.");
  Selective_Put_Line (+"[point 1] mmm = " & Mmm);
  Selective_Put_Line (5 * "Main ");
  T2.Gimmea_Num (Mmm);
  Selective_Put_Line (+"[point 2] mmm = " & Mmm);
  Selective_Put_Line (+"Done.");
  if compiler_regression_test_mode and then Mmm /= the_answer then
    HAL.Put_Line ("   ----> Compiler test failed.");
    HAL.Set_Exit_Status (1);
  end if;
end Tasks_02;
