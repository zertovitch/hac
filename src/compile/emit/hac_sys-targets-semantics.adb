package body HAC_Sys.Targets.Semantics is

  overriding procedure Initialize_Code_Emission (m : in out Machine) is
    use HAT;
  begin
    m.busy := True;
    m.started := Clock;
  end Initialize_Code_Emission;

  overriding procedure Finalize_Code_Emission
    (m       : in out Machine;
     strings :        String)
  is
    use HAT;
  begin
    m.finished := Clock;
    m.total_time := m.finished - m.started;
    m.busy := False;
  end Finalize_Code_Emission;

  ----------------------
  -- Mark_Declaration --
  ----------------------

  overriding procedure Mark_Declaration (m : Machine) is
  begin
    null;
  end Mark_Declaration;

  --------------------
  -- Mark_Reference --
  --------------------

  overriding procedure Mark_Reference (m : Machine) is
  begin
    null;
  end Mark_Reference;

end HAC_Sys.Targets.Semantics;
