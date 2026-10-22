-------------------------------------------
--  HAC  <->  Native data exchange demo  --
-------------------------------------------

--  Registration of callbacks on Native side.

with HAC_Sys.Builder;

package Exchange_Native_Side_Pkg is

  --  A "session" for the contextual call-backs: the same
  --  Next_Count_Callback can be registered with different
  --  sessions, and mutates each one separately.
  type Session_Counter is limited private;
  type Session_Access is access all Session_Counter;
  function Count (S : Session_Counter) return Integer;
  --  A fresh session, for the contextual call-backs.
  --  Heap-allocated: independent from any local scope.
  function New_Session return Session_Access;

  procedure Register_All_Callbacks
    (BD      : HAC_Sys.Builder.Build_Data;
     Session : Session_Access);

  procedure Set_Global_VM_Variable (BD : in out HAC_Sys.Builder.Build_Data);

  function Get_Global_VM_Variable (BD : HAC_Sys.Builder.Build_Data) return String;

private
  type Session_Counter is record
    count : Integer := 0;
  end record;
end Exchange_Native_Side_Pkg;
