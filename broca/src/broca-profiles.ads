with CORBA;

with Broca.Opaque;   use Broca.Opaque;
with Broca.Buffers;
with Broca.IOP;      use Broca.IOP;
with Broca.Sequences;

package Broca.Profiles is

   procedure Register
     (Tag                     : in Profile_Tag;
      Unmarshall_Profile_Body : in Unmarshall_Profile_Body_Type);
   --  Register Unmarshall_Profile_Body as the function used
   --  to unmarshall a Tagged Profile Body corresponding to Tag.

   --  procedure Unegister
   --    (Tag : in Profile_Tag);
   --  Remove any registered unmarshalling function associated
   --  with Tag.


   ------------------------
   --  Unknwon_Profile_Type
   ------------------------

   type Encapsulation_Ptr is access Broca.Buffers.Encapsulation;

   type Unknown_Profile_Type is new Profile_Type with
      record
         Tag  : Profile_Tag;
         Data : Encapsulation_Ptr
           := null;
      end record;

   type Unknown_Profile_Access is access Unknown_Profile_Type;

   function Get_Object_Key
     (Profile : Unknown_Profile_Type)
     return Broca.Sequences.Octet_Sequence;

   function Find_Connection
     (Profile : access Unknown_Profile_Type)
     return Connection_Ptr;

   function Get_Profile_Tag
     (Profile : Unknown_Profile_Type)
     return Profile_Tag;

   function Get_Profile_Priority
     (Profile : in Unknown_Profile_Type)
     return Profile_Priority;

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Unknown_Profile_Type);

--     function Unmarshall_Profile_Body
--       (Buffer  : access Buffers.Buffer_Type)
--        return Unknown_Profile_Access;

   procedure Finalize
     (X : in out Unknown_Profile_Type);

   ----------------------------
   --  Tagged component profile
   ----------------------------

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

   procedure Marshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type;
      Profile : Profile_Type'Class);
   --  Marshall a TaggedProfile into Buffer.

   function Unmarshall_Tagged_Profile
     (Buffer : access Buffers.Buffer_Type)
     return Profile_Ptr;
   --  Unmarshall a TaggedProfile from Buffer.
   --  The Profile_Type designated by the returned
   --  Profile_Ptr is dynamically allocated; it is up
   --  to the caller to release the associated storage
   --  when the profile is not needed anymore.

   -----------------------
   -- Object References --
   -----------------------

   procedure Encapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : in CORBA.String;
      Profiles : in Profile_Ptr_Array_Ptr);

   procedure Decapsulate_IOR
     (Buffer   : access Buffers.Buffer_Type;
      Type_Id  : out CORBA.String;
      Profiles : out Profile_Ptr_Array_Ptr;
      Used_Profile_Index : out CORBA.Unsigned_Long;
      Is_Supported_Profile : out Boolean);

end Broca.Profiles;
