with CORBA;
with Broca.Buffers; use Broca.Buffers;
with Broca.Refs;
with Broca.Sequences;

package Broca.Object is
   --  Contains data for a connection.
   type Connection_Type is abstract tagged null record;
   type Connection_Acc is access all Connection_Type'Class;
   function Get_Request_Id (Connection : access Connection_Type)
                            return CORBA.Unsigned_Long is abstract;
   --  Release a previously acquired connection.
   procedure Release_Connection (Connection : access Connection_Type) is
     abstract;

   --  Send a buffer to a connection.
   --  Raise comm_failure in case of error.
   procedure Send
     (Connection : access Connection_Type;
      Buffer     : in out Buffer_Descriptor) is abstract;

   --  Receive data from a connection.  Fill exactly Buffer. Can
   --  raise comm_failure.
   procedure Receive
     (Connection : access Connection_Type;
      Buffer     : in out Buffer_Descriptor) is abstract;

   type Profile_Type is abstract tagged limited null record;
   function Get_Object_Key (Profile : Profile_Type)
                            return Broca.Sequences.Octet_Sequence is abstract;
   --  Find a free connection (or create a new one) for a message to an
   --  OBJECT via PROFILE and reserve it.
   function Find_Connection (Profile : access Profile_Type)
                             return Connection_Acc is abstract;

   type Profile_Ptr is access all Profile_Type'Class;
   type Profile_Acc_Array is
      array (CORBA.Unsigned_Long range <>) of Profile_Ptr;
   type Profile_Acc_Array_Acc is access Profile_Acc_Array;

   type Object_Type is new Broca.Refs.Ref_Type with
      record
         Type_Id : CORBA.String;
         Profiles : Profile_Acc_Array_Acc;
      end record;

   type Object_Ptr is access all Object_Type;

   --  Find a profile for a message.
   function Find_Profile (Object : Object_Ptr) return Profile_Ptr;
end Broca.Object;
