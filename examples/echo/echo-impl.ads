with AdaBroker;
use AdaBroker;
with AdaBroker.Omniobject ;
with Corba ;
package Echo.Impl is

   type Object is new Omniobject.Implemented_Object with private ;
   type Object_Ptr is access all Object ;


   -----------------------
   -- IDL definitions   --
   -----------------------

   function echoString(Self : access Object; mesg : in Corba.String) return Corba.String ;




private

   -- You may add fields to this record
   type Object is new AdaBroker.Omniobject.Implemented_Object with record
      Null ;
   end record ;

   --------------------------------------------------
   ----          finalization operators          ----
   --------------------------------------------------
   procedure Initialize(Self : in out Object) ;
   procedure Adjust(Self : in out Object) ;
   procedure Finalize(Self : in out Object) ;

end Echo.Impl ;
