with AdaBroker.OmniORB ;
with Corba ;
package vehicle.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   function Get_mark(Self : access Object) return Corba.String ;

   procedure Set_mark(Self : access Object ;
                      To : in Corba.String ) ;

   function can_drive(Self : access Object; age : in Corba.Unsigned_Short) return Corba.Boolean ;




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

end vehicle.Impl ;
