--  Example binding data concrete implementation.

--  $Id$

package body Droopi.Binding_Data.Test is

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

   procedure Finalize (P : in out Local_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Finalize;

   function Get_Object_Key
     (Profile : Test_Profile)
     return Objects.Object_Id is
   begin
      return Profile.Oid.all;
   end Get_Object_Key;

   procedure Bind_Profile
     (Profile : Test_Profile;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access) is
   begin
      raise Not_Implemented;
   end Bind_Profile;

   function Get_Profile_Tag
     (Profile : Test_Profile)
     return Profile_Tag is
   begin
      raise Not_Implemented;
      return Get_Profile_Tag (Profile);
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : Test_Profile)
     return Profile_Preference is
   begin
      raise Not_Implemented;
      return Get_Profile_Reference (Profile);
   end Get_Profile_Reference;

   procedure Destroy (Profile : in out Profile_Type) is
   begin
      Free (Profile.Oid);
   end Destroy;

   type Test_Profile_Factory is new Profile_Factory with private;

   function Create_Profile
     (PF  : access Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access is
   begin
      return new Test_Profile'
        (Oid => new Object_Id'(Oid),
         Address => TAP_Address (TAP));
   end Create_Profile;

private

   type Test_Profile is new Test_Profile with null record;

   type Test_Profile_Factory is new Profile_Factory
      with null record;

end Droopi.Binding_Data;
