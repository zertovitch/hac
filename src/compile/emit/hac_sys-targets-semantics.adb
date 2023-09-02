package body HAC_Sys.Targets.Semantics is

  overriding procedure Initialize_Code_Emission (m : in out Machine) is
    use HAT;
  begin
    m.busy := True;
    m.started := Clock;
    m.ref_map.Clear;
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

  overriding procedure Mark_Declaration (m : in out Machine; is_built_in : Boolean) is
  begin
    m.decl_map (m.CD.Id_Count).is_built_in := is_built_in;
    if not is_built_in then
      m.decl_map (m.CD.Id_Count) :=
        (is_built_in => False,
         file_name   => m.CD.CUD.source_file_name,
         line        => m.CD.CUD.line_count,
         column      => m.CD.syStart + 1);
    end if;
  end Mark_Declaration;

  overriding procedure Mark_Reference (m : in out Machine; located_id : Natural) is
    use HAT;
  begin
    m.ref_map.Insert
      (Key =>
         --  Example: "c:\files\source.adb 130 12"
         m.CD.CUD.source_file_name &
         m.CD.CUD.line_count'Image &
         Integer'Image (m.CD.syStart + 1),
       New_Item =>
         located_id);
  end Mark_Reference;

  overriding procedure Find_Declaration
    (m          : in out Machine;
     point      : in out Declaration_Point;  --  in: reference; out: declaration
     located_id :    out Integer;
     found      :    out Boolean)

  is
    curs : Reference_Mapping.Cursor;
    use HAT, Reference_Mapping;
  begin
    curs :=
      m.ref_map.Find
        (point.file_name & point.line'Image & point.column'Image);
    if curs = No_Element then
      located_id := -1;
      found := False;
    else
      located_id := Element (curs);
      point := m.decl_map (located_id);
      found := True;
    end if;
  end Find_Declaration;

end HAC_Sys.Targets.Semantics;
