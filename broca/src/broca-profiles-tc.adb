with Ada.Unchecked_Deallocation;

with Broca.Sequences;
with Broca.CDR; use Broca.CDR;
with Broca.Exceptions;

package body Broca.Profiles.TC is

   ---------------------
   -- Find_Connection --
   ---------------------

   function Find_Connection
     (Profile : access Multiple_Component_Profile_Type)
     return Connection_Ptr is
   begin
      Broca.Exceptions.Raise_Internal;
      return null;
   end Find_Connection;

   ------------------
   -- Finalization --
   ------------------

   procedure Finalization
     (Profile : in out Multiple_Component_Profile_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Tagged_Component_Type, Tagged_Component_Access);
      procedure Free is new Ada.Unchecked_Deallocation
        (Tagged_Component_Array, Tagged_Components_Ptr);
   begin
      for I in Profile.Components'Range loop
         Free (Profile.Components (I));
      end loop;
      Free (Profile.Components);
   end Finalization;

   --------------------
   -- Get_Object_Key --
   --------------------

   function Get_Object_Key
     (Profile : Multiple_Component_Profile_Type)
     return Broca.Sequences.Octet_Sequence is
   begin
      Broca.Exceptions.Raise_Internal;
      return Broca.Sequences.Null_Sequence;
   end Get_Object_Key;

   --------------------------
   -- Get_Profile_Priority --
   --------------------------

   function Get_Profile_Priority
     (Profile : in Multiple_Component_Profile_Type)
     return Profile_Priority is
   begin
      return Profile_Priority'First;
   end Get_Profile_Priority;

   ---------------------
   -- Get_Profile_Tag --
   ---------------------

   function Get_Profile_Tag
     (Profile : Multiple_Component_Profile_Type)
      return Profile_Tag is
   begin
      return Tag_Multiple_Components;
   end Get_Profile_Tag;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Tagged_Component : Tagged_Component_Type)
   is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Tagged_Component.Tag));
      Marshall (Buffer, Tagged_Component.Component_Data);
   end Marshall;

   --------------
   -- Marshall --
   --------------

   procedure Marshall
     (Buffer  : access Buffers.Buffer_Type;
      Components : in Tagged_Component_Array)
   is
   begin
      Marshall (Buffer, CORBA.Unsigned_Long (Components'Length));
      for I in Components'Range loop
         Marshall (Buffer, Components (I).all);
      end loop;
   end Marshall;

   ---------------------------
   -- Marshall_Profile_Body --
   ---------------------------

   procedure Marshall_Profile_Body
     (Buffer  : access Buffers.Buffer_Type;
      Profile : Multiple_Component_Profile_Type) is
   begin
      Marshall (Buffer, Profile.Components.all);
   end Marshall_Profile_Body;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
      return Tagged_Component_Access
   is
      Tag : CORBA.Unsigned_Long := Unmarshall (Buffer);
      Data : Octet_Array := Unmarshall (Buffer);
   begin
      return new Tagged_Component_Type'
        (Encapsulation_Size => Data'Length,
         Tag => Component_Id (Tag),
         Component_Data => Data);
   end Unmarshall;

   ----------------
   -- Unmarshall --
   ----------------

   function Unmarshall
     (Buffer  : access Buffers.Buffer_Type)
     return Tagged_Components_Ptr
   is
      use CORBA;
      Length : CORBA.Unsigned_Long;
      Result : Tagged_Components_Ptr;
   begin
      Length := Unmarshall (Buffer);
      Result := new Tagged_Component_Array (0 .. Length - 1);
      for I in Result'Range loop
         Result (I) := Unmarshall (Buffer);
      end loop;
      return Result;
   end Unmarshall;

end Broca.Profiles.TC;
