with vehicle.Impl;
with CORBA.Object.OmniORB;
with tank.Skel;
with weapon.Impl;
with weapon;
with CORBA;
package body tank.Impl is 

   procedure shoot
     (Self : access Object;
      ranges : in weapon.dist)
   is
   begin 
      null;
   end shoot;

   function move
     (Self : access Object;
      wide : in weapon.dist)
      return CORBA.String
   is
   begin 
      return CORBA.To_CORBA_String ("Turn left");
   end move;

   -----------------------------------------------------------
   --  Implementations objects are controlled, you can add  --
   --  instructions in the following functions as specified --
   -----------------------------------------------------------

   procedure Initialize (Self : in out Object) is
   begin
      vehicle.Impl.Initialize
           (vehicle.Impl.Object (Self),
         tank.Repository_Id);
      -- Add user code *BELOW* this line
   end Initialize;

   procedure Adjust (Self: in out Object) is
   begin
      vehicle.Impl.Adjust (vehicle.Impl.Object (Self));
      -- Add user code *BELOW* this line
   end Adjust;

   procedure Finalize (Self : in out Object) is
   begin
      -- Add user code *BEFORE* this line
      vehicle.Impl.Finalize (vehicle.Impl.Object (Self));
   end Finalize;

begin
   CORBA.Object.OmniORB.Register
     (tank.Repository_Id,
      tank.Nil_Ref,
      tank.Skel.Dispatch'Access);
end tank.Impl;
