with HAC_Sys.Targets.HAC_Virtual_Machine;

with Ada.Unchecked_Deallocation;

package body HAC_Sys.Co_Defs is

  procedure Construct_Root (Root : out Exact_Typ; Typ : Typen) is
  begin
    Root.TYP      := Typ;
    Root.Ref      := 0;
    Root.Is_Range := False;
  end Construct_Root;

  function Construct_Root (Typ : Typen) return Exact_Typ is
    result : Exact_Typ;
  begin
    Construct_Root (result, Typ);
    return result;
  end Construct_Root;

  function Undefined return Exact_Typ is
  begin
    return Construct_Root (NOTYP);
  end Undefined;

  overriding procedure Construct_Root (Root : out Exact_Subtyp; Typ : Typen) is
  begin
    Construct_Root (Exact_Typ (Root), Typ);  --  Call parent method.
    Root.Discrete_First := HAC_Integer'First;
    Root.Discrete_Last  := HAC_Integer'Last;
  end Construct_Root;

  overriding function Construct_Root (Typ : Typen) return Exact_Subtyp is
    result : Exact_Subtyp;
  begin
    Construct_Root (result, Typ);
    return result;
  end Construct_Root;

  overriding function Undefined return Exact_Subtyp is
  begin
    return Construct_Root (NOTYP);
  end Undefined;

  procedure Set_Source_Stream (
    CUD        : in out Current_Unit_Data;
    s          : access Ada.Streams.Root_Stream_Type'Class;
    file_name  : in     String;
    start_line : in     Natural := 0
  )
  is
  begin
    CUD.compiler_stream  := Source_Stream_Access (s);
    CUD.source_file_name := HAT.To_VString (file_name);
    CUD.line_count       := start_line;
  end Set_Source_Stream;

  function Get_Source_Name (SD : Current_Unit_Data) return String is
  begin
    return HAT.VStr_Pkg.To_String (SD.source_file_name);
  end Get_Source_Name;

  procedure Unchecked_Free is
    new Ada.Unchecked_Deallocation
      (Targets.Machine'Class, Targets.Abstract_Machine_Reference);

  function Is_HAC_VM (CD : Compiler_Data) return Boolean
  is
    use Targets;
  begin
    return CD.target /= null
      and then CD.target.all in HAC_Virtual_Machine.Machine'Class;
  end Is_HAC_VM;

  procedure Set_Target
    (CD : in out Compiler_Data; new_target : Targets.Abstract_Machine_Reference)
  is
    use Targets;
  begin
    if new_target /= null then
      if CD.target /= null then
        Unchecked_Free (CD.target);
      end if;
      CD.target := new_target;
      --  Special case for the HAC VM:
      if new_target.all in HAC_Virtual_Machine.Machine'Class then
        HAC_Virtual_Machine.Machine (new_target.all).CD := CD'Unchecked_Access;
      end if;
    end if;
  end Set_Target;

  overriding procedure Initialize (CD : in out Compiler_Data) is
  begin
    --  Ensure the CD has a valid target from the beginning:
    CD.Set_Target (new Targets.HAC_Virtual_Machine.Machine);
  end Initialize;

  overriding procedure Finalize (CD : in out Compiler_Data) is
  begin
    Unchecked_Free (CD.target);
  end Finalize;

  function Discrete_Image
    (CD : Compiler_Data; value : HAC_Integer; Typ : Typen; Ref : Index) return String is
  begin
    case Typ is
      when Ints  => return HAC_Image (value);
      when Bools => return Boolean'Image (Boolean'Val (value));
      when Chars => return Character'Image (Character'Val (value));
      when Enums =>
        if value in 0 .. CD.IdTab (Ref).xtyp.Discrete_Last then
          return A2S (CD.IdTab (Ref + 1 + Integer (value)).name_with_case);
        else
          return "[invalid position: " &  HAC_Image (value) & ']';
        end if;
      when others => raise Program_Error with "Non-discrete type";
    end case;
  exception
    when Constraint_Error =>
      return "[invalid position: " &  HAC_Image (value) & ']';
  end Discrete_Image;

  function Discrete_Range_Image
    (CD : Compiler_Data; value_1, value_2 : HAC_Integer; Typ : Typen; Ref : Index) return String is
  begin
    return
      Discrete_Image (CD, value_1, Typ, Ref) &
      " .. " &
      Discrete_Image (CD, value_2, Typ, Ref);
  end Discrete_Range_Image;

  function Size_of (CD : Compiler_Data; Id_Index : Natural) return Positive is
    X : constant Exact_Subtyp := CD.IdTab (Id_Index).xtyp;
  begin
    case X.TYP is
      when Arrays =>
        return CD.Arrays_Table (X.Ref).Array_Size;
      when Records =>
        return CD.Blocks_Table (X.Ref).VSize;
      when others =>
        return 1;
    end case;
  end Size_of;

  procedure Increment_Nesting_or_Descending_Level (CD : in out Compiler_Data) is
  begin
    CD.CUD.use_hat_stack_top := CD.CUD.use_hat_stack_top + 1;
    if CD.CUD.use_hat_stack_top > 0 then
      --  "Inherit" the possible presence of a USE clause for HAT.
      CD.CUD.Use_HAT_Stack (CD.CUD.use_hat_stack_top) :=
        CD.CUD.Use_HAT_Stack (CD.CUD.use_hat_stack_top - 1);
    end if;
  end Increment_Nesting_or_Descending_Level;

  procedure Decrement_Nesting_or_Descending_Level (CD : in out Compiler_Data) is
  begin
    CD.CUD.use_hat_stack_top := CD.CUD.use_hat_stack_top - 1;
  end Decrement_Nesting_or_Descending_Level;

end HAC_Sys.Co_Defs;
