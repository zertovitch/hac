-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------

--  Callbacks on Native side.

with Exchange_Common;

with HAC_Sys.Interfacing;

with Ada.Numerics.Long_Real_Arrays,
     Ada.Text_IO;

package body Exchange_Native_Side_Pkg is
  use Ada.Text_IO;
  use HAC_Sys.Interfacing;

  procedure Parameterless_Callback (Dummy : in out HAC_Element_Array) is
  begin
    Put_Line ("      Native: Parameterless_Callback is speaking!");
  end Parameterless_Callback;

  procedure Hello_Callback (Data : in out HAC_Element_Array) is
  begin
    Put_Line ("      Native: HAC is saying: [" & To_Native (Data (1)) & ']');
  end Hello_Callback;

  procedure Ints_Callback (Data : in out HAC_Element_Array) is
    i, j, k : Integer;
  begin
    i := To_Native (Data (1));
    j := To_Native (Data (2));
    k := To_Native (Data (3));
    Put_Line
      ("      Native: HAC has sent me the numbers:" &
       Integer'Image (i) &
       Integer'Image (j) &
       Integer'Image (k));
  end Ints_Callback;

  procedure Floats_Callback (Data : in out HAC_Element_Array) is
    f, g, h : Long_Float;
  begin
    f := To_Native (Data (1));
    g := To_Native (Data (2));
    h := To_Native (Data (3));
    Put_Line
      ("      Native: HAC has sent me the numbers:" &
       Long_Float'Image (f) &
       Long_Float'Image (g) &
       Long_Float'Image (h));
  end Floats_Callback;

  procedure Hello_Callback_in_out (Data : in out HAC_Element_Array) is
  begin
    Put_Line ("      Native: HAC is saying: [" & To_Native (Data (1)) & ']');
    Data (1) := To_HAC ("No, I'm Native, you loser!");
  end Hello_Callback_in_out;

  procedure Ints_Callback_in_out (Data : in out HAC_Element_Array) is
    i : Integer;
  begin
    i := To_Native (Data (1));
    Put_Line
      ("      Native: HAC has sent me the number:" &
       Integer'Image (i) & "; I will send it back squared.");
    Data (1) := To_HAC (i ** 2);
  end Ints_Callback_in_out;

  procedure Floats_Callback_in_out (Data : in out HAC_Element_Array) is
    f : Long_Float;
  begin
    f := To_Native (Data (1));
    Put_Line
      ("      Native: HAC has sent me the number:" &
       Long_Float'Image (f) & "; I will send it back squared.");
    Data (1) := To_HAC (f ** 2);
  end Floats_Callback_in_out;

  procedure Composite_Callback (Data : in out HAC_Element_Array) is
    m_pos : constant :=  1;  --  Index for matrix 1
    n_pos : constant :=  5;  --  Index for matrix 2
    r_pos : constant :=  9;  --  Index for record 1
    o_pos : constant := 12;  --  Index for matrix 3
    use Ada.Numerics.Long_Real_Arrays;
    package LFIO is new Ada.Text_IO.Float_IO (Long_Float); use LFIO;
    procedure Show (m : Real_Matrix) is
    begin
      for i in m'Range (1) loop
        Put ("          ");
        for j in m'Range (2) loop
          Put (m (i, j), 2, 2, 0);
        end loop;
        New_Line;
      end loop;
    end Show;
    --
    m, n, o : Real_Matrix (1 .. 2, 1 .. 2);
    e : Exchange_Common.Animal;
    --
    function To_HAC is new To_HAC_Any_Enum (Exchange_Common.Animal);
    function To_Native is new To_Native_Any_Enum (Exchange_Common.Animal);
  begin
    e := To_Native (Data (r_pos + 2));
    Put_Line ("      Native: Enum = " & Exchange_Common.Animal'Image (e));
    m := ((To_Native (Data (m_pos)),     To_Native (Data (m_pos + 1))),
          (To_Native (Data (m_pos + 2)), To_Native (Data (m_pos + 3))));
    Put_Line ("      Native: Matrix m:");
    Show (m);
    n := ((To_Native (Data (n_pos)),     To_Native (Data (n_pos + 1))),
          (To_Native (Data (n_pos + 2)), To_Native (Data (n_pos + 3))));
    Put_Line ("      Native: Matrix n:");
    Show (n);

    o := m * n;
    Put_Line ("      Native: Matrix o = m * n:");
    Show (o);

    Data (o_pos .. o_pos + 3) :=
      (To_HAC (o (1, 1)), To_HAC (o (1, 2)),
       To_HAC (o (2, 1)), To_HAC (o (2, 2)));

    Data (r_pos)     := To_HAC (Integer'(To_Native (Data (r_pos))) ** 2);
    Data (r_pos + 1) := To_HAC ("I'm a Native message now (niarg niarg niarg)!");
    e := Exchange_Common.cat;
    Data (r_pos + 2) := To_HAC (e);
    Put_Line ("      Native: Enum = " & Exchange_Common.Animal'Image (e));
  end Composite_Callback;

  procedure Register_All_Callbacks (BD : HAC_Sys.Builder.Build_Data) is
  begin
    Register (BD, Parameterless_Callback'Access, "Parameterless_Callback");
    Register (BD, Hello_Callback'Access,         "Hello_Callback");
    Register (BD, Ints_Callback'Access,          "Ints_Callback");
    Register (BD, Floats_Callback'Access,        "Floats_Callback");
    Register (BD, Hello_Callback_in_out'Access,  "Hello_Callback_in_out");
    Register (BD, Ints_Callback_in_out'Access,   "Ints_Callback_in_out");
    Register (BD, Floats_Callback_in_out'Access, "Floats_Callback_in_out");
    Register (BD, Composite_Callback'Access,     "Composite_Callback");
  end Register_All_Callbacks;

  procedure Set_Global (BD : in out HAC_Sys.Builder.Build_Data) is
  begin
    Set_VM_Variable (BD, "Demo_Variable", "Native... ");
  end Set_Global;

  function Get_Global (BD : HAC_Sys.Builder.Build_Data) return String is
  begin
    return Get_VM_Variable (BD, "Demo_Variable");
  end Get_Global;

end Exchange_Native_Side_Pkg;
