--  Example binding data concrete implementation.

--  $Id$

with PolyORB.Sockets;

package PolyORB.Binding_Data.SRP is

   pragma Elaborate_Body;

   type SRP_Profile_Type is new Profile_Type with private;

   procedure Initialize (P : in out SRP_Profile_Type);
   procedure Adjust (P : in out SRP_Profile_Type);
   procedure Finalize (P : in out SRP_Profile_Type);

   function Get_Object_Key
     (Profile : SRP_Profile_Type)
     return Objects.Object_Id;

   procedure Bind_Profile
     (Profile : SRP_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access);

   function Get_Profile_Tag
     (Profile : SRP_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : SRP_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   type SRP_Profile_Factory is new Profile_Factory with private;

   procedure Create_Factory
     (PF : out SRP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access);

   function Create_Profile
     (PF  : access SRP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access;

   function Is_Local_Profile
     (PF : access SRP_Profile_Factory;
      P : Profile_Access)
     return Boolean;

   function Image (Prof : SRP_Profile_Type) return String;

private

   type SRP_Profile_Type is new Profile_Type with record
      Address   : Sockets.Sock_Addr_Type;
      Object_Id : Objects.Object_Id_Access;
   end record;

   type SRP_Profile_Factory is new Profile_Factory
     with record
        Address : Sockets.Sock_Addr_Type;
     end record;

end PolyORB.Binding_Data.SRP;
