--  Code generation example for Ada records

with HAT;

procedure Record_Code_Gen is
  use HAT;
  type Typ is (ints, floats);
  type Field is (a, b);
  --
  function Typ_Of (f : Field) return Typ is
  begin
    case f is
      when a => return ints;
      when b => return floats;
    end case;
  end;
  --
  f_ada : File_Type;
  t : Typ;
begin
  --  Write a simple Ada record:
  Create (f_ada, "$_simple_ada_record_snippet.ads");
  Put_Line (f_ada, "  type T1 is record");
  for f in Field loop
    t := Typ_Of (f);
    Put_Line
      (f_ada,
       To_Lower (+"    " & f'Image & " : " & t'Image & ';'));
  end loop;
  Put_Line (f_ada, "  end record;");
  Close (f_ada);
  --
  --  Here, other forms: C header, records in records vs.
  --  flat representation, record of arrays, ...
end Record_Code_Gen;
