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

   function Find_Connection
     (Profile : Local_Profile_Type)
     return Components.Component_Access;

   function Get_Profile_Tag
     (Profile : Local_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : Local_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

private

   type Object_Id_Access is access all Objects.Object_Id;
   type Local_Profile_Type is new Profile_Type with record
      Object_Id : Object_Id_Access;
   end record;

end Droopi.Binding_Data.Local;
