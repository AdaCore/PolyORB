with Ada.Unchecked_Deallocation;
with Ada.Tags;
with Broca.Locks;
with Broca.Exceptions;
with Broca.Object;

--  XXX
with Ada.Task_Identification; use Ada.Task_Identification;
--  XXX

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Refs is

   use Broca.Buffers;

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.refs");
   procedure O is new Broca.Debug.Output (Flag);

   Counter_Global_Lock : Broca.Locks.Mutex_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Ref_Type'Class, Ref_Ptr);

   -------------------
   -- Disable_Usage --
   -------------------

   procedure Disable_Usage (Obj : in out Ref_Type) is
   begin
      if Obj.Counter /= 0 then
         Broca.Exceptions.Raise_Internal (100);
      else
         Obj.Counter := -1;
      end if;
   end Disable_Usage;

   ---------------
   -- Inc_Usage --
   ---------------

   procedure Inc_Usage (Obj : in Ref_Ptr) is
   begin
      if Obj.Counter /= -1 then
         pragma Debug (O ("inc_usage: locking for " & Image (Current_Task)));
         Counter_Global_Lock.Lock;
         pragma Debug (O ("inc_usage: locked for " & Image (Current_Task)));

         Obj.Counter := Obj.Counter + 1;

         pragma Debug (O ("inc_usage: unlocking for " & Image (Current_Task)));
         Counter_Global_Lock.Unlock;
         pragma Debug (O ("inc_usage: unlocked for " & Image (Current_Task)));
      end if;
   end Inc_Usage;

   ---------------
   -- Dec_Usage --
   ---------------

   procedure Dec_Usage (Obj : in out Ref_Ptr) is
   begin
      if Obj.Counter /= -1 then
         begin
            pragma Debug
              (O ("dec_usage: locking for " & Image (Current_Task)));
            Counter_Global_Lock.Lock;
            pragma Debug
              (O ("dec_usage: locked for " & Image (Current_Task)));
         exception
            when others =>
               pragma Debug (O ("dec_usage: exception in Lock"));
               null;
         end;

         Obj.Counter := Obj.Counter - 1;

         pragma Debug (O ("dec_usage: unlocking for " & Image (Current_Task)));
         Counter_Global_Lock.Unlock;
         pragma Debug (O ("dec_usage: unlocked for " & Image (Current_Task)));
         if Obj.Counter = 0 then
            pragma Debug
              (O ("dec_usage: deallocate " &
                  Ada.Tags.External_Tag (Obj.all'Tag)));
            Free (Obj);
         end if;
      else
         pragma Debug (O ("dec_usage: Counter = -1"));
         null;
      end if;
   end Dec_Usage;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Ref) is
   begin
      null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Ref) is
   begin
      if Object.A_Ref /= null then
         Inc_Usage (Object.A_Ref);
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Ref) is
   begin
      if Object.A_Ref /= null then
         Dec_Usage (Object.A_Ref);
      end if;
   end Finalize;

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Ref_Type) is
   begin
      Broca.Exceptions.Raise_Marshal;
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Ref_Type) is
   begin
      Broca.Exceptions.Raise_Marshal;
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Value  : out Ref_Type) is
   begin
      raise Program_Error;
   end Unmarshall;

   ----------------------
   -- Compute_New_Size --
   ----------------------

   procedure Compute_New_Size
     (Buffer : in out Buffer_Descriptor;
      Value  : in Ref) is
   begin
      if Value.A_Ref = null then
         Broca.Exceptions.Raise_Marshal;
      end if;
      Compute_New_Size (Buffer, Value.A_Ref.all);
   end Compute_New_Size;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer : in out Buffer_Descriptor;
      Value  : in Ref) is
   begin
      if Value.A_Ref = null then
         Broca.Exceptions.Raise_Marshal;
      end if;
      Marshall (Buffer, Value.A_Ref.all);
   end Marshall;

   ----------------
   -- Unmarshall --
   ----------------

   procedure Unmarshall
     (Buffer : in out Buffer_Descriptor;
      Value  : out Ref)
   is
      New_Ref : Ref;
      Obj : constant Ref_Ptr
        := new Broca.Object.Object_Type;
   begin
      Unmarshall (Buffer, Obj.all);
      Set (New_Ref, Obj);
      Value := New_Ref;
   end Unmarshall;

   ---------
   -- Get --
   ---------

   function Get (Self : Ref) return Ref_Ptr is
   begin
      return Self.A_Ref;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (Self : in out Ref; Referenced : Ref_Ptr) is
   begin
      if Self.A_Ref /= null then
         Dec_Usage (Self.A_Ref);
      end if;
      Self.A_Ref := Referenced;
      if Referenced /= null then
         Inc_Usage (Referenced);
      end if;
   end Set;

end Broca.Refs;
