with CORBA;
with PortableServer;
with Broca.Buffers;
package all_types.Skel is

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

   function echoRef
     (Self : access Object;
      arg : in Ref)
     return Ref is abstract;

   function echoColor
     (Self : access Object;
      arg : in Color)
      return Color is abstract;

   --  procedure testException
   --    (Self : access Object;
   --     arg : in CORBA.Long) is abstract;

   function echoUnion
     (Self : access Object;
      arg : in myUnion)
      return myUnion is abstract;

   function echoArray
     (Self : access Object;
      arg : in simple_array)
      return simple_array is abstract;

   function echoMatrix
     (Self : access Object;
      arg : in matrix)
      return matrix is abstract;

   function echoStruct
     (Self : access Object;
      arg : in simple_struct)
      return simple_struct is abstract;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence)
      return U_sequence is abstract;

   function Get_Counter
     (Self : access Object)
      return CORBA.Long is abstract;

   function Get_myColor
     (Self : access Object)
      return Color is abstract;

   procedure Set_myColor
     (Self : access Object;
      To   : in Color) is abstract;

private
   function Get_Type_Id (Obj : Object) return CORBA.RepositoryId;
   procedure GIOP_Dispatch
     (Obj : access Object;
      Operation : String;
      Request_Id : CORBA.Unsigned_Long;
      Response_Expected : CORBA.Boolean;
      Request_Buffer : access Broca.Buffers.Buffer_Type;
      Reply_Buffer   : access Broca.Buffers.Buffer_Type);
end all_types.Skel;
