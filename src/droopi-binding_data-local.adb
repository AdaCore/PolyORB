--  Contact information for an object that exists
--  within the local ORB.

--  $Id$

with Ada.Unchecked_Deallocation;

package body Droopi.Binding_Data.Local is

   use Droopi.Objects;

   procedure Initialize (P : in out Local_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   procedure Adjust (P : in out Local_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   procedure Finalize (P : in out Local_Profile_Type)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object_Id, Object_Id_Access);
   begin
      Free (P.Object_Id);
   end Finalize;

   procedure Create_Local_Profile
     (Oid : Objects.Object_Id;
      P   : out Local_Profile_Type) is
   begin
      pragma Assert (P.Object_Id = null);
      P.Object_Id := new Object_Id'(Oid);
      pragma Assert (P.Object_Id /= null);
   end Create_Local_Profile;

   function Get_Object_Key
     (Profile : Local_Profile_Type)
     return Objects.Object_Id is
   begin
      return Profile.Object_Id.all;
   end Get_Object_Key;

   function Find_Connection
     (Profile : Local_Profile_Type)
     return Components.Component_Access is
   begin
      pragma Assert (False);
      return null;
      --  May not happen (no such a profile does not support
      --  connections).
   end Find_Connection;

   function Get_Profile_Tag
     (Profile : Local_Profile_Type)
     return Profile_Tag is
   begin
      return Tag_Local;
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : Local_Profile_Type)
     return Profile_Preference is
   begin
      return Profile_Preference'Last;
      --  A local profile is always preferred to any other.
   end Get_Profile_Preference;

end Droopi.Binding_Data.Local;
