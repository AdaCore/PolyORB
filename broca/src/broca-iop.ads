with CORBA;
with Broca.Buffers;
with Broca.Sequences;

package Broca.IOP is

   --  Contains data for a connection.
   type Connection_Type is abstract tagged null record;
   type Connection_Ptr is access all Connection_Type'Class;

   function Get_Request_Id
     (Connection : access Connection_Type)
     return CORBA.Unsigned_Long is abstract;

   procedure Release_Connection
     (Connection : access Connection_Type) is abstract;
   --  Release a previously acquired connection.

   procedure Send
     (Connection : access Connection_Type;
      Buffer     : in out Buffers.Buffer_Descriptor) is abstract;
   --  Send a buffer to a connection. Raise comm_failure on error.

   procedure Receive
     (Connection : access Connection_Type;
      Buffer     : in out Buffers.Buffer_Descriptor) is abstract;
   --  Receive data from a connection. Fill exactly Buffer. Raise
   --  comm_failure on error.

   subtype Profile_Id is CORBA.Unsigned_Long;
   Tag_Internet_IOP        : constant Profile_Id := 0;
   Tag_Multiple_Components : constant Profile_Id := 1;

   type Profile_Type is abstract tagged limited null record;

   function Get_Object_Key
     (Profile : Profile_Type)
     return Broca.Sequences.Octet_Sequence is abstract;
   --  Find a free connection (or create a new one) for a message to an
   --  Object via Profile and reserve it.

   function Find_Connection
     (Profile : access Profile_Type)
     return Connection_Ptr is abstract;

   function Get_Profile_Id
     (Profile : Profile_Type)
      return Profile_Id is abstract;

   type Profile_Ptr is access all Profile_Type'Class;

   type Profile_Ptr_Array is
     array (CORBA.Unsigned_Long range <>) of Profile_Ptr;

   type Profile_Ptr_Array_Ptr is access Profile_Ptr_Array;

   procedure Encapsulate_IOR
     (Buffer   : in out Buffers.Buffer_Descriptor;
      From     : in Buffers.Buffer_Index_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr);

   procedure Decapsulate_IOR
     (Buffer   : in out Buffers.Buffer_Descriptor;
      Type_Id  : out CORBA.String;
      Profiles : out Profile_Ptr_Array_Ptr);

   type Encapsulate_Profile_Type is
     access procedure
       (Buffer  : in out Buffers.Buffer_Descriptor;
        From    : in Buffers.Buffer_Index_Type;
        Profile : in Profile_Ptr);

   type Decapsulate_Profile_Type is
     access procedure
       (Buffer  : in out Buffers.Buffer_Descriptor;
        Profile : out Profile_Ptr);

   procedure Register
     (Profile     : in Profile_Id;
      Encapsulate : in Encapsulate_Profile_Type;
      Decapsulate : in Decapsulate_Profile_Type);

end Broca.IOP;
