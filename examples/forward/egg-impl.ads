with AdaBroker.OmniORB ;
with Chicken_forward ;
with CORBA.Boa ;
package Egg.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   function hatch(Self : access Object) return Chicken_forward.Ref ;

   --------------------------
   -- user-defined stuff   --
   --------------------------
   procedure Set_Boa(Self : in out Object ;
                     Boa : CORBA.Boa.Object) ;


private

   -- You may add fields to this record
   type Object is new AdaBroker.OmniORB.ImplObject with record
      Already : Boolean := False ;
      Boa : CORBA.Boa.Object ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end Egg.Impl ;
