--  Example binding data concrete implementation.

--  $Id$

with Ada.Streams; use Ada.Streams;

with CORBA;
with Droopi.CORBA_P.Exceptions;

with Droopi.Transport.Sockets;
with Droopi.Protocols.GIOP;
with Droopi.Protocols;
with Droopi.Representations.CDR;
with Droopi.Filters;
with Droopi.Filters.Slicers;
with Droopi.Sockets;
with Droopi.Objects;

--  The IIOP protocol is defined upon TCP/IP.

package body Droopi.Binding_Data.IIOP is


   use Droopi.Representations.CDR;
   use Droopi.Objects;
   use Droopi.Transport.Sockets;
   use Droopi.Sockets;


   procedure Marshall_Socket
        (Buffer   : access Buffer_Type;
         Sock     : Sockets.Sock_Addr_Type);

   procedure Unmarshall_Socket
    (Buffer   : access Buffer_Type;
     Sock     : out Sockets.Sock_Addr_Type);


   procedure Initialize (P : in out IIOP_Profile_Type) is
   begin
      P.Object_Id := null;
   end Initialize;

   procedure Adjust (P : in out IIOP_Profile_Type) is
   begin
      if P.Object_Id /= null then
         P.Object_Id := new Object_Id'(P.Object_Id.all);
      end if;
   end Adjust;

   procedure Finalize (P : in out IIOP_Profile_Type) is
   begin
      Free (P.Object_Id);
   end Finalize;

   function Get_Object_Key
     (Profile : IIOP_Profile_Type)
     return Objects.Object_Id is
   begin
      return Profile.Object_Id.all;
   end Get_Object_Key;


   procedure Bind_Profile
     (Profile : IIOP_Profile_Type;
      TE      : out Transport.Transport_Endpoint_Access;
      Filt    : out Components.Component_Access)
   is
      use Droopi.Components;
      use Droopi.Protocols;
      use Droopi.Protocols.GIOP;
      use Droopi.Sockets;
      use Droopi.Filters;
      use Droopi.Filters.Slicers;

      Sock : Socket_Type;
      Remote_Addr : Sock_Addr_Type := Profile.Address;
      Pro  : aliased GIOP_Protocol;
      Sli  : aliased Slicer_Factory;

   begin

      Create_Socket (Sock);
      Connect_Socket (Sock, Remote_Addr);
      TE := new Transport.Sockets.Socket_Endpoint;
      Create (Socket_Endpoint (TE.all), Sock);

      Chain_Factories ((0 => Sli'Unchecked_Access,
                        1 => Pro'Unchecked_Access));

      Filt := Component_Access (Create_Filter_Chain (Sli'Unchecked_Access));

      --  The caller will invoke Register_Endpoint on TE.
   end Bind_Profile;


   function Get_Profile_Tag
     (Profile : IIOP_Profile_Type)
     return Profile_Tag is
   begin
      return Tag_Internet_IOP;
   end Get_Profile_Tag;

   function Get_Profile_Preference
     (Profile : IIOP_Profile_Type)
     return Profile_Preference is
   begin
      return Preference_Default;
   end Get_Profile_Preference;

   procedure Create_Factory
     (PF : out IIOP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access) is
   begin
      PF.Address := Address_Of (Socket_Access_Point (TAP.all));
   end Create_Factory;

   function Create_Profile
     (PF  : access IIOP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      Oid : Objects.Object_Id)
     return Profile_Access
   is
      use Droopi.Transport.Sockets;

      Result : constant Profile_Access
        := new IIOP_Profile_Type;

      TResult : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);
   begin
      TResult.Object_Id := new Object_Id'(Oid);
      TResult.Address   := Address_Of
        (Socket_Access_Point (TAP.all));
      return  Result;
   end Create_Profile;

   function Is_Local_Profile
     (PF : access IIOP_Profile_Factory;
      P : Profile_Access) return Boolean is
   begin
      return P.all in IIOP_Profile_Type
        and then IIOP_Profile_Type (P.all).Address = PF.Address;
   end Is_Local_Profile;

   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   procedure Marshall_IIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile : Profile_Access)
   is

      IIOP_Profile : IIOP_Profile_Type renames IIOP_Profile_Type (Profile.all);
      Profile_Body : Buffer_Access;

   begin

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation.
      Start_Encapsulation (Profile_Body);

      --  Version
      Marshall (Profile_Body, CORBA.Octet (IIOP_Major_Version));
      Marshall (Profile_Body, CORBA.Octet (IIOP_Minor_Version));

      --  Marshalling of a Socket
      Marshall_Socket (Profile_Body, IIOP_Profile.Address);

      --  Marshalling of the Object Id
      Marshall (Profile_Body, Stream_Element_Array
                (IIOP_Profile.Object_Id.all));

      --  Marshall the Profile_Body into IOR.
      Marshall (Buf, Encapsulate (Profile_Body));
      Release (Profile_Body);

   end Marshall_IIOP_Profile_Body;


   --------------------------------
   -- Marshall_IIOP_Profile_Body --
   --------------------------------

   function Unmarshall_IIOP_Profile_Body
     (Buffer       : access Buffer_Type)
     return Profile_Access

   is
      use CORBA;
      use Droopi.CORBA_P.Exceptions;
      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : Buffer_Access := new Buffers.Buffer_Type;
      Major_Version  : CORBA.Octet;
      Minor_Version  : CORBA.Octet;
      Length         : CORBA.Long;
      Result         : Profile_Access := new IIOP_Profile_Type;
      TResult        : IIOP_Profile_Type
                       renames IIOP_Profile_Type (Result.all);


   begin
      Decapsulate (Profile_Body'Access, Profile_Buffer);

      Major_Version  := Unmarshall (Profile_Buffer);
      Minor_Version  := Unmarshall (Profile_Buffer);

      if Major_Version /=  IIOP_Major_Version
        or else Minor_Version > IIOP_Minor_Version
      then
         Release (Profile_Buffer);
         Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

      Unmarshall_Socket (Profile_Buffer, TResult.Address);

      declare
            Str  : aliased Stream_Element_Array :=
                     Unmarshall (Profile_Buffer);
      begin

            TResult.Object_Id := new Object_Id'(Object_Id (Str));
            if Minor_Version /= 0 then
               Length := Unmarshall (Profile_Buffer);
               if Length /= 0 then
                  Release (Profile_Buffer);
                  --  FIXME: Multiple components are not yet handled.
                  Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
               end if;
            end if;
      end;
      Release (Profile_Buffer);
      return Result;

   end Unmarshall_IIOP_Profile_Body;



   procedure Marshall_Socket
       (Buffer   : access Buffer_Type;
        Sock     : Sockets.Sock_Addr_Type)

   is
      use CORBA;
      Str  : CORBA.String := To_CORBA_String (Image (Sock.Addr));
   begin

      --  Marshalling of the Host as a string
      Marshall (Buffer, Str);

      --  Marshalling of the port
      Marshall (Buffer, CORBA.Unsigned_Short (Sock.Port));

   end Marshall_Socket;


   procedure Unmarshall_Socket
    (Buffer   : access Buffer_Type;
     Sock     : out Sockets.Sock_Addr_Type)

   is
      use CORBA;
      Str  : CORBA.String := Unmarshall (Buffer);
      Port : CORBA.Unsigned_Short;
   begin

      --  Unmarshalling of the Host
      Sock.Addr := Inet_Addr (To_Standard_String (Str));

      --  Unmarshalling of the port
      Port := Unmarshall (Buffer);
      Sock.Port := Port_Type (Port);

   end Unmarshall_Socket;





end Droopi.Binding_Data.IIOP;
