with Ada.Unchecked_Deallocation;
with Ada.Tags;
with Broca.Locks;
with Broca.Exceptions;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Refs is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.refs");
   procedure O is new Broca.Debug.Output (Flag);

   Counter_Global_Lock : Broca.Locks.Mutex_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Ref_Type'Class, Ref_Acc);

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

   procedure Inc_Usage (Obj : Ref_Acc) is
   begin
      if Obj.Counter /= -1 then
         Counter_Global_Lock.Lock;
         Obj.Counter := Obj.Counter + 1;
         Counter_Global_Lock.Unlock;
      end if;
   end Inc_Usage;

   procedure Dec_Usage (Obj : in out Ref_Acc) is
   begin
      if Obj.Counter /= -1 then
         Counter_Global_Lock.Lock;
         Obj.Counter := Obj.Counter - 1;
         Counter_Global_Lock.Unlock;
         if Obj.Counter = 0 then
            pragma Debug (O ("dec_usage: deallocate " &
                             Ada.Tags.External_Tag (Obj.all'Tag)));
            Free (Obj);
         end if;
      end if;
   end Dec_Usage;

   procedure Initialize (Object : in out Ref) is
   begin
      null;
   end Initialize;

   procedure Adjust (Object : in out Ref) is
   begin
      if Object.A_Ref /= null then
         Inc_Usage (Object.A_Ref);
      end if;
   end Adjust;

   procedure Finalize (Object : in out Ref) is
   begin
      if Object.A_Ref /= null then
         Dec_Usage (Object.A_Ref);
      end if;
   end Finalize;

   function Object_To_IOR (Obj : Ref_Type) return Broca.Buffers.Buffer_Descriptor
   is
      Res : Broca.Buffers.Buffer_Descriptor;
   begin
      Broca.Exceptions.Raise_Marshal;
      return Res;
   end Object_To_IOR;

   function Get (Self : Ref) return Ref_Acc is
   begin
      return Self.A_Ref;
   end Get;

   procedure Set (Self : in out Ref; Referenced : Ref_Acc) is
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
