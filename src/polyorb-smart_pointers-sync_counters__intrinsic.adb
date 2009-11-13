separate (PolyORB.Smart_Pointers) package body Sync_Counters is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32
   is
      function Intrinsic_Sync_Add_And_Fetch
        (Ptr   : access Interfaces.Integer_32;
         Value : Interfaces.Integer_32) return Interfaces.Integer_32;
      pragma Import
        (Intrinsic, Intrinsic_Sync_Add_And_Fetch, "__sync_add_and_fetch_4");

   begin
      return Intrinsic_Sync_Add_And_Fetch (Ptr, Value);
   end Sync_Add_And_Fetch;

end Sync_Counters;
