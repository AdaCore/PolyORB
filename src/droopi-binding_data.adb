--  Management of binding data, i. e. the elements of information
--  that designate a remote middleware TSAP.

--  $Id$

with Ada.Unchecked_Deallocation;

package body Droopi.Binding_Data is

   procedure Destroy_Profile (P : in out Profile_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Profile_Type'Class, Profile_Access);
   begin
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
