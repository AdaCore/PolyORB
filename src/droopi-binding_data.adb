--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id$

with Ada.Tags;
with Ada.Unchecked_Deallocation;

with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body Droopi.Binding_Data is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.binding_data");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Destroy_Profile (P : in out Profile_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Profile_Type'Class, Profile_Access);
   begin
      pragma Assert (P /= null);

      pragma Debug
        (O ("Destroying profile of type "
            & Ada.Tags.External_Tag (P'Tag)));

      Free (P);
   end Destroy_Profile;

   function Get_Binding_Object
     (Profile : Profile_Type)
     return Components.Component_Access is
   begin
      return Profile.Binding_Object;
   end Get_Binding_Object;

   procedure Set_Binding_Object
     (Profile : in out Profile_Type;
      BO      :        Components.Component_Access) is
   begin
      Profile.Binding_Object := BO;
   end Set_Binding_Object;

end Droopi.Binding_Data;
