with CORBA;
with PortableServer;
with Broca.Types;
package all_types.Impl is

   type Object is abstract new PortableServer.Servant_Base
     with null record;

   function echoBoolean
     (Self : access Object;
      arg : in CORBA.Boolean)
      return CORBA.Boolean is abstract;

   function echoShort
     (Self : access Object;
      arg : in CORBA.Short)
      return CORBA.Short is abstract;

   function echoLong
     (Self : access Object;
      arg : in CORBA.Long)
      return CORBA.Long is abstract;

   function echoUShort
     (Self : access Object;
      arg : in CORBA.Unsigned_Short)
      return CORBA.Unsigned_Short is abstract;

   function echoULong
     (Self : access Object;
      arg : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is abstract;

   function echoFloat
     (Self : access Object;
      arg : in CORBA.Float)
      return CORBA.Float is abstract;

   function echoDouble
     (Self : access Object;
      arg : in CORBA.Double)
      return CORBA.Double is abstract;

   function echoChar
     (Self : access Object;
      arg : in CORBA.Char)
      return CORBA.Char is abstract;

   function echoOctet
     (Self : access Object;
      arg : in CORBA.Octet)
      return CORBA.Octet is abstract;

   function echoString
     (Self : access Object;
      arg : in CORBA.String)
      return CORBA.String is abstract;

private
   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId;
   procedure Giop_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Reponse_Expected : CORBA.Boolean;
      Stream : in out Broca.Types.Buffer_Descriptor);
end all_types.Impl;
