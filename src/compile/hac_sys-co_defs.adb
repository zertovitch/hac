with HAC_Sys.Targets.HAC_Virtual_Machine;

with Ada.Unchecked_Deallocation;

package body HAC_Sys.Co_Defs is

  procedure Construct_Root (Root : out Exact_Typ; Typ : Typen) is
  begin
    Root.TYP      := Typ;
    Root.Ref      := 0;
    Root.Is_Range := False;
  end Construct_Root;

  overriding procedure Construct_Root (Root : out Exact_Subtyp; Typ : Typen) is
  begin
    Construct_Root (Exact_Typ (Root), Typ);  --  Call parent method.
    Root.Discrete_First := HAC_Integer'First;
    Root.Discrete_Last  := HAC_Integer'Last;
  end Construct_Root;

  function Construct_Root (Typ : Typen) return Exact_Subtyp is
    result : Exact_Subtyp;
  begin
    Construct_Root (result, Typ);
    return result;
  end Construct_Root;

  procedure Elevate_to_Maybe (item : in out No_Maybe_Yes) is
  begin
    --  no    -> maybe
    --  maybe -> maybe
    --  yes   -> yes
    item := No_Maybe_Yes'Max (item, maybe);
  end Elevate_to_Maybe;

  procedure Elevate_to_Maybe_or_Yes (item : in out No_Maybe_Yes; context : Flow_Context) is
  begin
    if context.is_within_condition or context.is_within_loop then
      --  The condition might not be fullfilled, the loop might be skipped.
      Elevate_to_Maybe (item);
    else
      item := yes;
    end if;
  end Elevate_to_Maybe_or_Yes;

  procedure Set_Source_Stream
    (CUD        : in out Current_Unit_Data;
     s          : in     Source_Stream_Access;
     file_name  : in     String;         --  Can be a virtual name (editor title, zip entry)
     start_line : in     Natural := 0)
  is
  begin
    CUD.compiler_stream  := s;
    CUD.source_file_name := HAT.To_VString (file_name);
    CUD.location.line    := start_line;
  end Set_Source_Stream;

  function Get_Source_Name (CUD : Current_Unit_Data) return String is
  begin
    return HAT.VStr_Pkg.To_String (CUD.source_file_name);
  end Get_Source_Name;

  function Source_Buffer_has_Data (CUD : Current_Unit_Data) return Boolean is
  (CUD.buffer_length > 0);

  procedure Unchecked_Free is
    new Ada.Unchecked_Deallocation
      (Targets.Machine'Class, Targets.Abstract_Machine_Reference);

  function Is_Executable (CD : Compiler_Data) return Boolean is
  begin
    return CD.main_proc_id_index /= HAC_Sys.Co_Defs.No_Id;
  end Is_Executable;

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
      --  Special case for a special target: the HAC VM.
      if new_target.all in HAC_Virtual_Machine.Machine'Class then
        HAC_Virtual_Machine.Machine (new_target.all).CD := CD'Unchecked_Access;
      end if;
    end if;
  end Set_Target;

  procedure Set_File_Catalogue
    (CD  : in out Compiler_Data;
     cat : in     Files.Abstract_File_Catalogue_Reference)
  is
  begin
    CD.cat := cat;
  end Set_File_Catalogue;

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
        if value in 0 .. CD.id_table (Ref).xtyp.Discrete_Last then
          return A2S (CD.id_table (Ref + 1 + Integer (value)).name_with_case);
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
    X : constant Exact_Subtyp := CD.id_table (Id_Index).xtyp;
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

begin
  undefined_subtyp.Construct_Root (NOTYP);
end HAC_Sys.Co_Defs;
