with Omniobject ;
package weapon.Impl is

   type Object is new Omniobject.Implemented_Object with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   procedure shoot(Self : access Object; ranges : in dist ) ;



private

   -- You may add fields to this record
   type Object is new Omniobject.Implemented_Object with record
      Null ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end weapon.Impl ;
