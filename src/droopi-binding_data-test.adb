--  Example binding data concrete implementation.

--  $Id$

with Droopi.Filters;

with Droopi.Protocols.Echo;
--  The TEST profile is bound to the Echo invocation protocol.

with Droopi.Transport.Sockets;
--  The TEST profile denotes an Echo protocol stack instanciated
--  over a TCP socket.

package body Droopi.Binding_Data.Test is

   use Droopi.Objects;

   procedure Initialize (P : in out Test_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   procedure Adjust (P : in out Test_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   procedure Finalize (P : in out Test_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Finalize;

   function Get_Object_Key
     (Profile : Test_Profile_Type)
     return Objects.Object_Id is
   begin
      return Profile.Object_Id.all;
   end Get_Object_Key;

   procedure Bind_Profile
     (Profile : Test_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Session : out Components.Component_Access)
   is
      use Droopi.Protocols.Echo;
      use Droopi.Sockets;
      use Droopi.Transport.Sockets;

      S : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      P : aliased Echo_Protocol;

   begin
      Create_Socket (S);
      Connect_Socket (S, Remote_Addr);
      TE := new Transport.Sockets.Socket_Endpoint;
      Create (Socket_Endpoint (TE.all), S);
      Create (P'Access, Filters.Filter_Access (Session));
   end Bind_Profile;

   function Get_Profile_Tag
     (Profile : Test_Profile_Type)
     return Profile_Tag is
   begin
      return Tag_Test;
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : Test_Profile_Type)
     return Profile_Preference is
   begin
      return Preference_Default;
   end Get_Profile_Preference;

   function Create_Profile
     (PF  : access Test_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access
   is
      use Droopi.Transport.Sockets;

      Result : constant Profile_Access
        := new Test_Profile_Type;

      TResult : Test_Profile_Type
        renames Test_Profile_Type (Result.all);
   begin
      TResult.Object_Id := new Object_Id'(Oid);
      TResult.Address   := Address_Of
        (Socket_Access_Point (TAP.all));
      return  Result;
   end Create_Profile;

end Droopi.Binding_Data.Test;
