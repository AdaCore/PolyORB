with AdaBroker.OmniORB ;
with CORBA ;
package Echo.Impl is

   type Object is new AdaBroker.OmniORB.ImplObject with private;
   type Object_Ptr is access all Object;


   function echoString(Self : access Object; mesg : in CORBA.String) return CORBA.String ;




private

   -- You may add fields to this record
   type Object is new AdaBroker.OmniORB.ImplObject with record
      Null;
   end record;

   procedure Initialize (Self : in out Object);
   procedure Adjust     (Self : in out Object);
   procedure Finalize   (Self : in out Object);

end Echo.Impl;
