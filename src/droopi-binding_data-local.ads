--  Contact information for an object that exists
--  within the local ORB.

--  $Id$

with Droopi.Objects;

package Droopi.Binding_Data.Local is

   pragma Elaborate_Body;

   type Local_Profile_Type is new Profile_Type with private;

   procedure Initialize (P : in out Local_Profile_Type);
   procedure Adjust (P : in out Local_Profile_Type);
   procedure Finalize (P : in out Local_Profile_Type);

   procedure Create_Local_Profile
     (Oid : Objects.Object_Id;
      P   : out Local_Profile_Type);

   function Get_Object_Key
     (Profile : Local_Profile_Type)
     return Objects.Object_Id;

   procedure Bind_Profile
     (Profile : Local_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access);

   function Get_Profile_Tag
     (Profile : Local_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : Local_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   --  Since Local profiles are not associated with any
   --  transport endpoint, there is no need to define
   --  an associated Profile_Factory.

private

   type Local_Profile_Type is new Profile_Type with record
      Object_Id : Objects.Object_Id_Access;
   end record;

end Droopi.Binding_Data.Local;
