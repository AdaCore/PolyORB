--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id: //droopi/main/src/droopi-binding_data.ads#1 $

with Ada.Finalization;
with Ada.Streams;

with Droopi.Protocols;
with Droopi.Types;

package Droopi.Binding_Data is

   ----------------------------------------------
   -- Abstract inter-ORB protocol profile type --
   ----------------------------------------------

   type Profile_Type is abstract
     new Ada.Finalization.Limited_Controlled with private;
   type Profile_Access is access all Profile_Type'Class;

   --  A profile is an element of information that contains:
   --    - a profile tag identifying a communication system and a
   --      method invocation protocol stack;
   --    - an unambiguous name of one TSAP within the communication
   --      system;
   --    - a key that identifies one object among all those accessible
   --      on that TSAP;
   --    - a priority, locally assigned, that denotes the preferrence
   --      expressed by the user for the choice of a profile type
   --      among a set of profiles.

   --  subtype Profile_Tag is CORBA.Unsigned_Long;
   subtype Profile_Tag is Types.Unsigned_Long;

   Tag_Internet_IOP        : constant Profile_Tag;
   Tag_Multiple_Components : constant Profile_Tag;

   type Profile_Priority is new Integer range 0 .. Integer'Last;
   --  Profile_Priority'First means "unsupported profile type"

   function Get_Object_Key
     (Profile : Profile_Type)
     return Ada.Streams.Stream_Element_Array is abstract;
   --  Retrieve the opaque object key from Profile.

   function Find_Connection
     (Profile : Profile_Type)
     return Protocols.Session_Access is abstract;
   --  Find or create a session (or create a new one) that matches
   --  this profile in order to send a message to the designated
   --  transport endpoint.

   function Get_Profile_Tag
     (Profile : Profile_Type)
     return Profile_Tag is abstract;
   pragma Inline (Get_Profile_Tag);
   --  Return the profile tag associated with this profile type.

   function Get_Profile_Priority
     (Profile : Profile_Type)
     return Profile_Priority is abstract;
   pragma Inline (Get_Profile_Priority);
   --  Return the profile priority associated with this profile type.

private

   Tag_Internet_IOP        : constant Profile_Tag := 0;
   Tag_Multiple_Components : constant Profile_Tag := 1;

   type Profile_Type is abstract
     new Ada.Finalization.Limited_Controlled with null record;

end Droopi.Binding_Data;
