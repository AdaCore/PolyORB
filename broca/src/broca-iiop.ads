with Interfaces.C;
with CORBA;
with Broca.Sequences;
with Broca.Object;
with Broca.Buffers; use Broca.Buffers;
with Broca.Locks;
with Sockets.Thin;
with Interfaces.C;

package Broca.IIOP is
   type Version_Type is
      record
         Major : CORBA.Octet;
         Minor : CORBA.Octet;
      end record;

   --  Simply linked list of strands.
   --  A strand is simply a connection with associated data.
   type Strand_Type;
   type Strand_Acc is access Strand_Type;
   type Strand_Type is
      record
         Next : Strand_Acc;
         --  File descriptor for the connection.
         Fd : Interfaces.C.int;
         --  Request id for the next request.
         Request_Id : CORBA.Unsigned_Long;
         --  A client is using this strand and waiting for a reply.
         Lock : Broca.Locks.Mutex_Type;
      end record;

   type Profile_Iiop_Type is new Broca.Object.Profile_Type with
      record
         --  Informations directly taken from the IOR.
         Iiop_Version : Version_Type;
         Host : CORBA.String;
         Port : CORBA.Unsigned_Short;
         Network_Port : Interfaces.C.unsigned_short;
         Object_Key : Broca.Sequences.Octet_Sequence;
         --  Components: Natural;

         --  The address corresponding to host/port.
         Socket_Address : Sockets.Thin.Sockaddr_In;

         --  List of strands.
         Strands : Strand_Acc := null;

         --  Lock on the list of strands.
         Lock : Broca.Locks.Rw_Lock_Type;
      end record;
   type Profile_Iiop_Acc is access Profile_Iiop_Type;

   --  Find a free connection (or create a new one) for a message to an
   --  OBJECT via PROFILE.
   function Find_Connection
     (Profile : access Profile_Iiop_Type)
      return Broca.Object.Connection_Acc;

   procedure Create_Profile
     (Buffer : in out Buffer_Descriptor;
      Profile : out Broca.Object.Profile_Ptr);

private
   function Get_Object_Key (Profile : Profile_Iiop_Type)
                            return Broca.Sequences.Octet_Sequence;

end Broca.IIOP;
