--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id: //droopi/main/src/droopi-binding_data.ads#8 $

with Ada.Finalization;

with Droopi.Components;
with Droopi.Objects;
with Droopi.Transport;
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
   Tag_Test                : constant Profile_Tag;

   type Profile_Preference is new Integer range 0 .. Integer'Last;
   --  Profile_Preference'First means "unsupported profile type"

   Preference_Default : constant Profile_Preference;
   --  Default value for profile preference.

   function Get_Object_Key
     (Profile : Profile_Type)
     return Objects.Object_Id
      is abstract;
   --  Retrieve the opaque object key from Profile.

   procedure Bind_Profile
     (Profile : Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access)
      is abstract;
   --  Find or create a transport endpoint and an attached protocol
   --  stack instance (or create new ones) that match this profile,
   --  in order to send a message to the designated middleware.
   --  The Transport_Endpoint at the bottom of the transport stack
   --  and the Session at the top are returned.

   function Get_Profile_Tag
     (Profile : Profile_Type)
     return Profile_Tag
      is abstract;
   pragma Inline (Get_Profile_Tag);
   --  Return the profile tag associated with this profile type.

   function Get_Profile_Preference
     (Profile : Profile_Type)
     return Profile_Preference
      is abstract;
   pragma Inline (Get_Profile_Preference);
   --  Return the profile priority associated with this profile type.

   type Profile_Factory is abstract tagged limited private;
   type Profile_Factory_Access is access all Profile_Factory'Class;

   procedure Create_Factory
     (PF : out Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access)
      is abstract;
   --  Initialize PF to act as profile factory for TAP.

   function Create_Profile
     (PF  : access Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access
      is abstract;
   --  Create a profile of the type determined by PF, using
   --  the address of TAP, and Oid as the object specification.

   procedure Destroy_Profile (P : in out Profile_Access);
   pragma Inline (Destroy_Profile);

   function Is_Local_Profile
     (PF : access Profile_Factory;
      P : Profile_Access)
     return Boolean is abstract;
   --  True iff P designates an object that can be contacted
   --  at the access point associated with PF.

private

   --  Standard tags defined by CORBA

   Tag_Internet_IOP        : constant Profile_Tag := 0;
   Tag_Multiple_Components : constant Profile_Tag := 1;

   --  Tags defined by DROOPI

   Tag_Local               : constant Profile_Tag := 16#7fffff00#;
   Tag_Test                : constant Profile_Tag := 16#7fffff01#;

   Preference_Default : constant Profile_Preference
     := (Profile_Preference'First + Profile_Preference'Last) / 2;

   type Profile_Type is abstract
     new Ada.Finalization.Limited_Controlled with null record;

   type Profile_Factory is abstract tagged limited null record;

end Droopi.Binding_Data;
