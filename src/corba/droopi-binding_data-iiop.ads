--  Example binding data concrete implementation.

--  $Id$

with CORBA;

with Droopi.Buffers; use Droopi.Buffers;
with Droopi.Sockets;


package Droopi.Binding_Data.IIOP is

   type IIOP_Profile_Type is new Profile_Type with private;


   procedure Initialize;

   procedure Initialize (P : in out IIOP_Profile_Type);
   procedure Adjust     (P : in out IIOP_Profile_Type);
   procedure Finalize   (P : in out IIOP_Profile_Type);

   function Get_Object_Key
     (Profile : IIOP_Profile_Type)
     return Objects.Object_Id;

   procedure Bind_Profile
     (Profile   : IIOP_Profile_Type;
      TE        : out Transport.Transport_Endpoint_Access;
      Filter    : out Components.Component_Access);

   function Get_Profile_Tag
     (Profile : IIOP_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : IIOP_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   type IIOP_Profile_Factory is new Profile_Factory with private;

   procedure Create_Factory
     (PF : out IIOP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access);

   function Create_Profile
     (PF  : access IIOP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access;

   function Is_Local_Profile
     (PF : access IIOP_Profile_Factory;
      P : Profile_Access) return Boolean;

   procedure Marshall_IIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile : Profile_Access);

   function   Unmarshall_IIOP_Profile_Body
     (Buffer   : access Buffer_Type)
    return  Profile_Access;

   function Image (Prof : IIOP_Profile_Type) return String;

private

   type IIOP_Profile_Type is new Profile_Type with record
      Address   : Sockets.Sock_Addr_Type;
      Object_Id : Objects.Object_Id_Access;
   end record;

   type IIOP_Profile_Factory is new Profile_Factory with record
      Address : Sockets.Sock_Addr_Type;
   end record;

   IIOP_Major_Version : constant CORBA.Octet := 1;

   IIOP_Minor_Version : constant CORBA.Octet := 2;

end Droopi.Binding_Data.IIOP;
