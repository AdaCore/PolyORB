with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Tags;
with Broca.Locks;
with Broca.Exceptions;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Refs is
   Flag : constant Natural := Broca.Debug.Is_Active ("broca.orb");
   procedure O is new Broca.Debug.Output (Flag);

   procedure Disable_Usage (Obj : in out Ref_Type) is
   begin
      if Obj.Counter /= 0 then
         Broca.Exceptions.Raise_Internal (100);
      else
         Obj.Counter := -1;
      end if;
   end Disable_Usage;

   Counter_Global_Lock : Broca.Locks.Mutex_Type;

   procedure Inc_Usage (Obj : Ref_Acc) is
   begin
      if Obj.Counter /= -1 then
         Counter_Global_Lock.Lock;
         Obj.Counter := Obj.Counter + 1;
         Counter_Global_Lock.Unlock;
      end if;
   end Inc_Usage;

   procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
     (Object => Ref_Type'Class, Name => Ref_Acc);

   procedure Dec_Usage (Obj : in out Ref_Acc) is
   begin
      if Obj.Counter /= -1 then
         Counter_Global_Lock.Lock;
         Obj.Counter := Obj.Counter - 1;
         Counter_Global_Lock.Unlock;
         if Obj.Counter = 0 then
            Ada.Text_IO.Put ("dec_usage: deallocate ");
            Ada.Text_IO.Put_Line (Ada.Tags.External_Tag (Obj.all'Tag));
            Unchecked_Deallocation (Obj);
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

   function Object_To_IOR (Obj : Ref_Type) return Broca.Types.Buffer_Descriptor
   is
      Res : Broca.Types.Buffer_Descriptor;
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
