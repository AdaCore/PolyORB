with Broca.Opaque;
with Broca.Sequences;

package Broca.Profiles.TC is

   pragma Elaborate_Body;

   use Broca.Opaque;
   
   type Component_Id is new CORBA.Unsigned_Long;

   type Tagged_Component_Type (Encapsulation_Size : Positive_Index_Type) is
      record
         Tag : Component_Id;
         Component_Data : Octet_Array (1 .. Encapsulation_Size);
      end record;

   type Tagged_Component_Access is access Tagged_Component_Type;

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Tagged_Component : Tagged_Component_Type);

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
      return Tagged_Component_Access;

   type Tagged_Component_Array is
      array (CORBA.Unsigned_Long range <>) of Tagged_Component_Access;

   type Tagged_Components_Ptr is access Tagged_Component_Array;

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Components : in Tagged_Component_Array);

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Tagged_Components_Ptr;

   type Multiple_Component_Profile_Type is new Profile_Type with
     record
        Components : Tagged_Components_Ptr := null;
     end record;

   procedure Finalization
     (Profile : in out Multiple_Component_Profile_Type);

   function Get_Object_Key
     (Profile : Multiple_Component_Profile_Type)
     return Broca.Sequences.Octet_Sequence;

   function Find_Connection
     (Profile : access Multiple_Component_Profile_Type)
     return Connection_Ptr;

   function Get_Profile_Tag
     (Profile : Multiple_Component_Profile_Type)
      return Profile_Tag;

   function Get_Profile_Priority
     (Profile : in Multiple_Component_Profile_Type)
     return Profile_Priority;

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Multiple_Component_Profile_Type);

end Broca.Profiles.TC;
