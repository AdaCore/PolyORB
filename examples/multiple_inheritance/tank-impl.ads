with AdaBroker.OmniORB ;
with vehicle.Impl ;
with weapon.Impl ;
with Corba ;
package tank.Impl is

   type Object is new vehicle.Impl.Object with private ;
   type Object_Ptr is access all Object ;


   -----------------------------
   -- inheritance from weapon
   -----------------------------

   procedure shoot(Self : access Object; ranges : in dist ) ;



   -----------------------
   -- IDL definitions   --
   -----------------------

   function move(Self : access Object; fast : in Weapon.dist) return Corba.String ;




private

   -- You may add fields to this record
   type Object is new vehicle.Impl.Object with record
      Null;
   end record;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end tank.Impl ;
