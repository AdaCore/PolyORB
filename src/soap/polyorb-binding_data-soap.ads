
with PolyORB.Types;
with PolyORB.Sockets;
with PolyORB.Objects;
with PolyORB.Protocols.HTTP; use PolyORB.Protocols.HTTP;

package PolyORB.Binding_Data.SOAP is

   pragma Elaborate_Body;

   Default_Port : constant := 80;


   type SOAP_Profile_Type is new Profile_Type with private;
   type SOAP_Profile_Factory is new Profile_Factory with private;

   procedure Initialize (P : in out SOAP_Profile_Type);
   procedure Adjust     (P : in out SOAP_Profile_Type);
   procedure Finalize   (P : in out SOAP_Profile_Type);


   function Get_Object_Key
     (Profile : SOAP_Profile_Type)
     return Objects.Object_Id;
   --  Retrieve the opaque object key from Profile.

   function Get_URL
     (Profile : SOAP_Profile_Type)
     return URL_Object;

   procedure Bind_Profile
     (Profile   : SOAP_Profile_Type;
      TE        : out Transport.Transport_Endpoint_Access;
      Filter    : out Components.Component_Access);

   function Get_Profile_Tag
     (Profile : SOAP_Profile_Type)
     return Profile_Tag;
   pragma Inline (Get_Profile_Tag);
   --  Return the profile tag associated with this profile type.

   function Get_Profile_Preference
     (Profile : SOAP_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);
   --  Return the profile priority associated with this profile type.


   procedure Create_Factory
     (PF : out SOAP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access);

   function Create_Profile
     (PF  : access SOAP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access;

   function Create_Profile
     (PF  : access SOAP_Profile_Factory;
      URL : Types.String)
     return Profile_Access;

   function Is_Local_Profile
     (PF : access SOAP_Profile_Factory;
      P : Profile_Access) return Boolean;

   function Image (Prof : SOAP_Profile_Type) return String;

   function Mapping_String_To_Object
     (S : Types.String)
     return Objects.Object_Id;

   function Mapping_Object_To_String
     (O : Objects.Object_Id)
     return Types.String;

private

   Tag_SOAP  : constant Profile_Tag :=  16#7fffff01#;

   type SOAP_Profile_Type is new Profile_Type with record
      Address       : Sockets.Sock_Addr_Type;
      Target_URL    : Types.String;
      Object_Id     : Objects.Object_Id_Access;
   end record;

   type SOAP_Profile_Factory is new Profile_Factory with record
      Address     : Sockets.Sock_Addr_Type;
      Target_URL  : Types.String;
   end record;

end PolyORB.Binding_Data.SOAP;
