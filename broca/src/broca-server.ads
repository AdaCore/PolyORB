with CORBA;
with Broca.Types; use Broca.Types;
with Broca.Poa;
with Broca.Stream;

package Broca.Server is
   --  Log.
   --  This is displayed/stored only if broca.flags.log is true.
   procedure Log (S : String);

   --  A server can receive and send messages.
   --  It can do this by managing connections, eg from inet.
   --
   --  Every server must derive of type server_type, and must register itself
   --  with the register procedure.
   type Server_Type is abstract tagged null record;

   --  Put a message into BUFFER.  Currently, this is a blocking operation, ie,
   --  it must put a message.
   --  BUFFER can already have been allocated, as a result it can be reused,
   --  or freed and replaced.
   procedure Perform_Work (Server : access Server_Type;
                           Buffer : in out Broca.Types.Buffer_Descriptor)
      is abstract;

   --  During the building of an IOR, this procedure is called to know the
   --  length of the profile for the object designed by INDEX.
   --  Length can be null, if the server can't create a profile.
   --  Need only to update IOR.POS.
   --  OBJECT_KEY must be 4-aligned
   procedure Marshall_Size_Profile (Server : access Server_Type;
                                    Ior : in out Broca.Types.Buffer_Descriptor;
                                    Object_Key : Broca.Types.Buffer_Descriptor)
      is abstract;
   --  During the building of an IOR, the procedure is called to marshall a
   --  profile.  The length of the profile (ie, the number added to IOR.POS)
   --  must be the same as that of MARSHALL_SIZE_PROFILE.
   --  In particular, it must be zero for no profile.
   --  OBJECT_KEY must be 4-aligned
   procedure Marshall_Profile (Server : access Server_Type;
                               Ior : in out Broca.Types.Buffer_Descriptor;
                               Object_Key : Broca.Types.Buffer_Descriptor)
      is abstract;

   type Server_Acc is access all Server_Type'Class;

   --  This private type is identify a server.  It acts like a cookie for
   --  a server.
   type Server_Id_Type is private;

   --  A server, in order to be active, must register itself with this
   --  procedure.
   procedure Register (Server : Server_Acc; Id : out Server_Id_Type);

   --  This procedure is called by a POA to request a server task to perform
   --  arbitrary work, such as cleaning the POA up.
   procedure Request_Cleanup (Poa : Broca.Poa.POA_Object_Access);

   --  When a server has a request that can be processed, it must inform
   --  with this procedure.
   --  POS is the identifier coming from register.
   procedure New_Request (Id : Server_Id_Type);

   --  This procedure is designed to be called by perform_work primitive to
   --  process a message.
   procedure Handle_Message (Stream : Broca.Stream.Stream_Acc;
                             Buffer : in out Buffer_Descriptor);

   --  Register a POA.
   --  broca.poa.all_poas_lock should have been lock_w.
   procedure Register_POA (Poa : Broca.Poa.POA_Object_Access);

   --  Unregister a POA.
   --  broca.poa.all_poas_lock should have been lock_w.
   procedure Unregister_POA (Poa : Broca.Poa.POA_Object_Access);

   --  This procedure builds an IOR.
   --  It can return a null_string if there is no profiles for this object.
   --  KEY is only the key for the POA, not the full object key.
   procedure Build_Ior (Target : out Broca.Types.Buffer_Descriptor;
                        Type_Id : CORBA.RepositoryId;
                        Poa : Broca.Poa.POA_Object_Access;
                        Key : Broca.Types.Buffer_Descriptor);
private
   type Server_Id_Type is new Natural;
end Broca.Server;
