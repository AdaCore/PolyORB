--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id: //droopi/main/src/droopi-binding_data.ads#4 $

with Ada.Finalization;

with Droopi.Components;
with Droopi.Objects;
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
   Tag_Local               : constant Profile_Tag;

   type Profile_Preference is new Integer range 0 .. Integer'Last;
   --  Profile_Preference'First means "unsupported profile type"

   function Get_Object_Key
     (Profile : Profile_Type)
     return Objects.Object_Id is abstract;
   --  Retrieve the opaque object key from Profile.

   function Find_Connection
     (Profile : Profile_Type)
     return Components.Component_Access is abstract;
   --  Find or create a session component (or create a new one) that matches
   --  this profile in order to send a message to the designated
   --  transport endpoint.

   function Get_Profile_Tag
     (Profile : Profile_Type)
     return Profile_Tag is abstract;
   pragma Inline (Get_Profile_Tag);
   --  Return the profile tag associated with this profile type.

   function Get_Profile_Preference
     (Profile : Profile_Type)
     return Profile_Preference is abstract;
   pragma Inline (Get_Profile_Preference);
   --  Return the profile priority associated with this profile type.

private

   --  Standard tags defined by CORBA

   Tag_Internet_IOP        : constant Profile_Tag := 0;
   Tag_Multiple_Components : constant Profile_Tag := 1;

   --  Tags defined by DROOPI

   Tag_Local               : constant Profile_Tag := 16#7fffff00#;

   type Profile_Type is abstract
     new Ada.Finalization.Limited_Controlled with null record;

end Droopi.Binding_Data;
