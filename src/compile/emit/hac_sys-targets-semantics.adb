with HAC_Sys.Defs;

with Ada.Unchecked_Deallocation;

package body HAC_Sys.Targets.Semantics is

  overriding procedure Finalize (m : in out Machine) is
    procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation
        (Declaration_Line_Maps.Map, Declaration_Line_Map_Access);
  begin
    for line_map_a of m.loc_map loop
      Unchecked_Free (line_map_a);
    end loop;
  end Finalize;

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

  trace : constant Boolean := False;

  overriding procedure Mark_Declaration (m : in out Machine; is_built_in : Boolean) is
    use File_Names_to_Line_Maps_Maps;
    curs : Cursor;
    line_map_a : Declaration_Line_Map_Access;
  begin
    --  Feed the declaration array used for getting a declaration's coordinates
    --  from an index in the identifier table, possibly obtained
    --  via Find_Referenced_Declaration.
    --
    m.decl_array (m.CD.Id_Count) :=
      (file_name   => (if is_built_in then HAT.Null_VString else m.CD.CUD.source_file_name),
       line        => (if is_built_in then -1               else m.CD.CUD.location.line),
       column      => (if is_built_in then -1               else m.CD.CUD.location.column_start),
       is_built_in => is_built_in,
       id_index    => m.CD.Id_Count,
       others      => <>);

    --  Feed the structures for searching possible declarations
    --  at any given point of any source file.
    --
    if not is_built_in then
      --  1) Find or create the line map associated to the file name.
      curs := m.loc_map.Find (m.CD.CUD.source_file_name);
      if curs = No_Element then
        line_map_a := new Declaration_Line_Maps.Map;
        m.loc_map.Insert (m.CD.CUD.source_file_name, line_map_a);
      else
        line_map_a := Element (curs);
      end if;
      --  2) Insert the infos for the declaration into the line map,
      --     possibly replacing a value for the same line number.
      line_map_a.Include (m.CD.CUD.location.line, m.CD.Id_Count);
    end if;
  end Mark_Declaration;

  procedure Add_Reference
    (m          : in out Machine;
     ref        : in     Reference_Point'Class;
     located_id : in     Natural)
  is
    use HAT;
    admit_duplicates : constant Boolean := True;
    key : constant VString :=
      ref.file_name & ref.line'Image & ref.column'Image;
      --  ^ Example: "c:\files\source.adb 130 12"
  begin
    if trace then
      HAT.Put_Line
        ("Mark_Reference, key = [" & key &
         "] for Id" & located_id'Image & ", " &
         Defs.A2S (m.CD.IdTab (located_id).name_with_case));
    end if;
    if admit_duplicates then
      m.ref_map.Include (key, located_id);
    else
      m.ref_map.Insert (key, located_id);
    end if;
  exception
    when Constraint_Error =>
      raise Constraint_Error
        with "Duplicate reference key: " & To_String (key);
  end Add_Reference;

  overriding procedure Mark_Spec_Body_Cross_References
    (m                : in out Machine;
     spec_id, body_id : in     Positive)
  is
  begin
    m.decl_array (spec_id).body_id := body_id;
    m.decl_array (body_id).spec_id := spec_id;
    --
    --  Add references to own declarations so an user can go from spec
    --  to body, and vice-versa, directly from the declarations.
    Add_Reference (m, m.decl_array (spec_id), spec_id);
    Add_Reference (m, m.decl_array (body_id), body_id);
  end Mark_Spec_Body_Cross_References;

  overriding procedure Mark_Reference (m : in out Machine; located_id : Natural) is
  begin
    Add_Reference
      (m,
       Reference_Point'
         (m.CD.CUD.source_file_name,
          m.CD.CUD.location.line,
          m.CD.CUD.location.column_start),
       located_id);
  end Mark_Reference;

  procedure Find_Referenced_Declarations
    (m              : in     Machine;
     ref            : in     Reference_Point'Class;
     decl_1, decl_2 :    out Declaration_Point'Class;
     found          :    out Natural)
  is
    use Co_Defs, HAT, Reference_Mapping;
    key  : constant VString :=
      ref.file_name & ref.line'Image & ref.column'Image;
    curs : constant Reference_Mapping.Cursor := m.ref_map.Find (key);
    index_1, index_2 : Positive;
  begin
    if curs = No_Element then
      decl_1.id_index := -1;
      decl_2.id_index := -1;
      found := 0;
    else
      index_1 := Element (curs);
      decl_1 := Declaration_Point'Class (m.decl_array (index_1));
      if decl_1.spec_id = No_Id and then decl_1.body_id = No_Id then
        found := 1;
      else
        found := 2;
        --  There is a cross-reference.
        --  In any case, decl_1 will be the spec and decl_2 will be the body.
        if decl_1.spec_id /= No_Id then
          --  We have a body referencing a spec.
          index_2 := index_1;
          index_1 := decl_1.spec_id;
          decl_1 := Declaration_Point'Class (m.decl_array (index_1));
        else
          --  We have a spec referencing a body.
          index_2 := decl_1.body_id;
        end if;
        decl_2 := Declaration_Point'Class (m.decl_array (index_2));
        --  Now, mark cases where ref = decl.
        if Reference_Point (decl_1) = Reference_Point (ref) then
          decl_1.self_reference := True;
        end if;
        if Reference_Point (decl_2) = Reference_Point (ref) then
          decl_2.self_reference := True;
        end if;
      end if;
    end if;
  end Find_Referenced_Declarations;

  function Find_Possible_Declarations
    (m        : Machine;
     point    : Reference_Point'Class;
     prefix   : String;
     max_list : Positive;
     max_scan : Positive) return String
  is
    use Co_Defs, HAT;
    line_map_a : Declaration_Line_Map_Access;
    idx, l : Natural;
    package Declaration_Lists is new Ada.Containers.Ordered_Maps (VString, VString);
    list : Declaration_Lists.Map;
    up_prefix : constant VString := To_Upper (+prefix);
    result : VString;
  begin
    --  1) Find the line map associated to the given file name.
    declare
      use File_Names_to_Line_Maps_Maps;
      curs : constant Cursor := m.loc_map.Find (point.file_name);
    begin
      if curs = No_Element then
        return "";
      else
        line_map_a := Element (curs);
      end if;
    end;
    --  2) Find the last declaration before given line.
    declare
      use Declaration_Line_Maps;
      curs : Cursor;
    begin
      curs := line_map_a.Floor (point.line - 1);
      if curs = No_Element then
        return "";
      else
        idx := Element (curs);
      end if;
    end;
    --  3) Build an identifier list matching the prefix.
    if trace then
      Put_Line
        (+"Find_Possible_Declarations, identifiers matching """ &
         prefix & """:");
    end if;
    for scan in 1 .. max_scan loop
      exit when idx = No_Id;
      exit when Integer (list.Length) = max_list;
      declare
        item : IdTabEntry renames m.CD.IdTab (idx);
      begin
        if Starts_With (item.name, up_prefix) then
          --  Enter the identifier in a case-insensitive way:
          list.Include (item.name, item.name_with_case);
          if trace then
            Put_Line (+"    " & item.name_with_case);
          end if;
        end if;
        --  Jump to previous identifier index, on same or lower level:
        idx := item.link;
      end;
    end loop;
    --  Output the list as a single string seperated by spaces:
    for s of list loop
      --  Like `result := result & s & ' ';`, possibly more efficient:
      VStr_Pkg.Append (result, s);
      VStr_Pkg.Append (result, ' ');
    end loop;
    l := Length (result);
    if l > 0 then
      Delete (result, l, l);  --  Remove trailing ' '
    end if;
    if trace then
      Put_Line (+"    [" & result & "]");
    end if;
    return To_String (result);
  end Find_Possible_Declarations;

end HAC_Sys.Targets.Semantics;
