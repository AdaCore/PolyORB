with AdaBroker.OmniORB ;
package weapon.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   procedure shoot(Self : access Object; ranges : in dist ) ;



private

   -- You may add fields to this record
   type Object is new AdaBroker.OmniORB.ImplObject with record
      Null ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end weapon.Impl ;
