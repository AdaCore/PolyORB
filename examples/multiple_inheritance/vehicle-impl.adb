with CORBA.Object.OmniORB;
with vehicle.Skel;
with CORBA; use CORBA;

package body vehicle.Impl is 

   Mark : CORBA.String;

   function Get_mark
     (Self : access Object)
      return CORBA.String
   is
   begin
      return Mark;
   end Get_mark;

   procedure Set_mark
     (Self : access Object;
      To   : in CORBA.String)
   is
   begin
      Mark := To;
   end Set_mark;

   function can_drive
     (Self : access Object;
      age : in CORBA.Unsigned_Short)
      return CORBA.Boolean
   is
   begin 
      return Age > 17;
   end can_drive;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      AdaBroker.OmniORB.Initialize
        (AdaBroker.OmniORB.ImplObject (Self),
         vehicle.Repository_Id);
      -- Add user code *BELOW* this line
   end Initialize;

   procedure Adjust (Self: in out Object) is
   begin
      AdaBroker.OmniORB.Adjust
        (AdaBroker.OmniORB.ImplObject (Self));
      -- Add user code *BELOW* this line
   end Adjust;

   procedure Finalize (Self : in out Object) is
   begin
      -- Add user code *BEFORE* this line
      AdaBroker.OmniORB.Finalize
        (AdaBroker.OmniORB.ImplObject (Self));
   end Finalize;

begin
   CORBA.Object.OmniORB.Register
     (vehicle.Repository_Id,
      vehicle.Nil_Ref,
      vehicle.Skel.Dispatch'Access);
end vehicle.Impl;
