with CORBA;
with PortableServer;

package Echo.Impl is

   type Object is new PortableServer.Servant_Base with null record;

   type Object_Acc is access Object;

   function EchoString
     (Self : access Object;
      Mesg : CORBA.String) return CORBA.String;

end Echo.Impl;
