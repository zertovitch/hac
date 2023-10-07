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
    --  Feed the declaration array.
    m.decl_array (m.CD.Id_Count) :=
      (file_name   => (if is_built_in then HAT.Null_VString else m.CD.CUD.source_file_name),
       line        => (if is_built_in then -1               else m.CD.CUD.location.line),
       column      => (if is_built_in then -1               else m.CD.CUD.location.column_start + 1),
       is_built_in => is_built_in,
       id_index    => m.CD.Id_Count);
    --  Feed the structures for searching declarations.
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

  overriding procedure Mark_Reference (m : in out Machine; located_id : Natural) is
    use HAT;
    admit_duplicates : constant Boolean := True;
    key : constant VString :=
      --  Example: "c:\files\source.adb 130 12"
      m.CD.CUD.source_file_name &
      m.CD.CUD.location.line'Image &
      Integer'Image (m.CD.CUD.location.column_start + 1);
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
      raise Constraint_Error with "Duplicate reference key: " & To_String (key);
  end Mark_Reference;

  procedure Find_Referenced_Declaration
    (m         : in     Machine;
     ref       : in     Reference_Point'Class;
     decl      :    out Declaration_Point'Class;
     was_found :    out Boolean)
  is
    curs : Reference_Mapping.Cursor;
    use HAT, Reference_Mapping;
  begin
    curs :=
      m.ref_map.Find
        (ref.file_name & ref.line'Image & ref.column'Image);
    if curs = No_Element then
      decl.id_index := -1;
      was_found := False;
    else
      decl := Declaration_Point'Class (m.decl_array (Element (curs)));
      was_found := True;
    end if;
  end Find_Referenced_Declaration;

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
    --  1) Find the line map associated to the file name.
    declare
      use File_Names_to_Line_Maps_Maps;
      curs : Cursor;
    begin
      curs := m.loc_map.Find (point.file_name);
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
