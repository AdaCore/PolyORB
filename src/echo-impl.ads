----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with PortableServer.POA;
with CORBA.ServerRequest;
with CORBA;
with PortableServer;

package Echo.Impl is

   type Object is
     new PortableServer.DynamicImplementation with private;

   type Object_Ptr is access all Object'Class;

   function echoString
     (Self : access Object;
      Mesg : in CORBA.String)
     return CORBA.String;

   procedure Invoke
     (Self : access Object;
      Request : in CORBA.ServerRequest.Object_ptr);

   function Primary_Interface (Self : access Object; -- ....
      POA_Ptr : PortableServer.POA.Ref) return String;

private

   type Object is
     new PortableServer.DynamicImplementation with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end Echo.Impl;
