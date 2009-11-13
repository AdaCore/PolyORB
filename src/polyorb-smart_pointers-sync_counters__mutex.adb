with PolyORB.Tasking.Mutexes; use PolyORB.Tasking.Mutexes;

separate (PolyORB.Smart_Pointers) package body Sync_Counters is

   Counter_Lock : Mutex_Access;
   --  Global lock used to protect concurrent accesses to reference counters

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create (Counter_Lock);
   end Initialize;

   ------------------------
   -- Sync_Add_And_Fetch --
   ------------------------

   function Sync_Add_And_Fetch
     (Ptr   : access Interfaces.Integer_32;
      Value : Interfaces.Integer_32) return Interfaces.Integer_32
   is
      Result : Interfaces.Integer_32;
   begin
      Enter (Counter_Lock);
      Ptr.all := Ptr.all + Value;
      Result := Ptr.all;
      Leave (Counter_Lock);
      return Result;
   end Sync_Add_And_Fetch;

end Sync_Counters;
