with AdaBroker.OmniORB;
with vehicle.Impl;
with weapon.Impl;
with weapon;
with CORBA;
package tank.Impl is

   type Object is new vehicle.Impl.Object with private;

   type Object_Ptr is access all Object;

   procedure shoot
     (Self : access Object;
      ranges : in weapon.dist);

   function move
     (Self : access Object;
      wide : in weapon.dist)
      return CORBA.String;

private

   type Object is new vehicle.Impl.Object with record
      null;
      -- Insert user declarations
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end tank.Impl;
