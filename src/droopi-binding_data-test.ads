--  Example binding data concrete implementation.

--  $Id$

with Droopi.Sockets;

package Droopi.Binding_Data.Test is

   type Test_Profile_Type is new Profile_Type with private;

   procedure Initialize (P : in out Test_Profile_Type);
   procedure Adjust (P : in out Test_Profile_Type);
   procedure Finalize (P : in out Test_Profile_Type);

   function Get_Object_Key
     (Profile : Test_Profile_Type)
     return Objects.Object_Id;

   procedure Bind_Profile
     (Profile : Test_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access);

   function Get_Profile_Tag
     (Profile : Test_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : Test_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   procedure Destroy (Profile : in out Test_Profile_Type);

   type Test_Profile_Factory is new Profile_Factory with private;

   function Create_Profile
     (PF  : access Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access;

private

   type Test_Profile_Type is new Profile_Type with record
      Address : Sockets.Sock_Addr_Type;
      Oid : Object_Id_Access;
   end record;

   type Test_Profile_Factory is new Profile_Factory
      with null record;

end Droopi.Binding_Data;
