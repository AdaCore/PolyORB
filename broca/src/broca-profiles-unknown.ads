with Broca.Sequences;

package Broca.Profiles.Unknown is

   pragma Elaborate_Body;
   
   type Encapsulation_Ptr is access Broca.Buffers.Encapsulation;

   type Unknown_Profile_Type is new Profile_Type with record
      Tag  : Profile_Tag;
      Data : Encapsulation_Ptr := null;
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

   procedure Finalize
     (X : in out Unknown_Profile_Type);

end Broca.Profiles.Unknown;
