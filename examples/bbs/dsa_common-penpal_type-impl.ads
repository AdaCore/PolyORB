with CORBA;
with PortableServer;

package DSA_Common.Penpal_Type.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure Initialize
     (Self : access Object;
      Name : in CORBA.String);

   function Name_Of
     (Self : access Object)
     return CORBA.String;

   procedure New_Message
     (Self : access Object;
      Sender : in CORBA.String;
      Message : in CORBA.String);

private

   type Object is new PortableServer.Servant_Base with record
      Name : CORBA.String;
   end record;

end DSA_Common.Penpal_Type.Impl;
