with Ada.Unchecked_Deallocation;

with Broca.CDR;        use Broca.CDR;
with Broca.Exceptions;

package body Broca.Profiles.Unknown is

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (X : in out Unknown_Profile_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Broca.Buffers.Encapsulation, Encapsulation_Ptr);
   begin
      Free (X.Data);
   end Finalize;

   ---------------------
   -- Find_Connection --
   ---------------------

   function Find_Connection
     (Profile : access Unknown_Profile_Type)
     return Connection_Ptr is
   begin
      Broca.Exceptions.Raise_Bad_Param;
      return null;
   end Find_Connection;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key
     (Profile : Unknown_Profile_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      Broca.Exceptions.Raise_Bad_Param;
      return Broca.Sequences.Null_Sequence;
   end Get_Object_Key;

   --------------------------
   -- Get_Profile_Priority --
   --------------------------

   function Get_Profile_Priority
     (Profile : in Unknown_Profile_Type)
     return Profile_Priority is
   begin
      return Profile_Priority'First;
   end Get_Profile_Priority;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : Unknown_Profile_Type)
     return Profile_Tag is
   begin
      return Profile.Tag;
   end Get_Profile_Tag;

   ---------------------------
   -- Marshall_Profile_Body --
   ---------------------------

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Unknown_Profile_Type) is
   begin
      Marshall (Buffer, Profile.Data.all);
   end Marshall_Profile_Body;

end Broca.Profiles.Unknown;
