with Omniobject ;
with Egg_forward ;
with Corba.Boa ;
package Chicken.Impl is

   type Object is new Omniobject.Implemented_Object with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   procedure lay(Self : access Object; number : out Corba.Unsigned_Short; Returns : out Egg_forward.Ref ) ;


   --------------------------
   -- user-defined stuff   --
   --------------------------
   procedure Set_Boa(Self : in out Object ;
                     Boa : Corba.Boa.Object) ;



private

   -- You may add fields to this record
   type Object is new Omniobject.Implemented_Object with record
      Number : Corba.Unsigned_Short := 0 ;
      Boa : Corba.Boa.Object ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end Chicken.Impl ;
