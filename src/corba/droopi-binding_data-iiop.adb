--  Example binding data concrete implementation.

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Transport.Sockets;
with Droopi.Protocols.GIOP;
with Droopi.Protocols;
with Droopi.Representations.CDR;
with Droopi.Filters;
with Droopi.Filters.Slicers;
with Droopi.Sockets;
with Droopi.Objects;
with Droopi.References.IOR;
with Droopi.Types;

with Sequences.Unbounded;

package body Droopi.Binding_Data.IIOP is


   use Droopi.Representations.CDR;
   use Droopi.Objects;
   use Droopi.Transport.Sockets;
   use Droopi.Sockets;
   use Droopi.References.IOR;
   use Droopi.Types;

   procedure Marshall_Socket
        (Buffer   : access Buffer_Type;
         Sock     : Sockets.Sock_Addr_Type);

   procedure Unmarshall_Socket
    (Buffer   : access Buffer_Type;
     Sock     : out Sockets.Sock_Addr_Type);


   procedure Initialize is
   begin
      Register
       (Tag_Internet_IOP,
        Marshall_IIOP_Profile_Body'Access,
        Unmarshall_IIOP_Profile_Body'Access);
   end Initialize;

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
      Filter  : out Components.Component_Access)
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
      Prof : Profile_Access := new IIOP_Profile_Type;
      --  This Profile_Access is stored in the created
      --  GIOP_Session, and free'd when the session is finalised.

      TProf : IIOP_Profile_Type
        renames IIOP_Profile_Type (Prof.all);

   begin
      Create_Socket (Sock);
      Connect_Socket (Sock, Remote_Addr);
      TE := new Transport.Sockets.Socket_Endpoint;
      Create (Socket_Endpoint (TE.all), Sock);

      Chain_Factories ((0 => Sli'Unchecked_Access,
                        1 => Pro'Unchecked_Access));

      Filter := Component_Access (Create_Filter_Chain (Sli'Unchecked_Access));
      --  Filter must be an access to the lowest filter in
      --  the stack (the slicer in the case of GIOP).

      TProf.Address := Profile.Address;
      TProf.Object_Id := Profile.Object_Id;
      Adjust (TProf);

      declare
         S : GIOP_Session
           renames GIOP_Session
           (Upper (Filter_Access (Filter)).all);
      begin
         Store_Profile (S'Access, Prof);
         Set_Version
           (S'Access,
            Profile.Major_Version,
            Profile.Minor_Version);
      end;

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
      use Component_Seq;

      Result : constant Profile_Access
        := new IIOP_Profile_Type;

      TResult : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);
   begin
      TResult.Object_Id := new Object_Id'(Oid);
      TResult.Address   := Address_Of
        (Socket_Access_Point (TAP.all));
      TResult.Components := Null_Sequence;
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
      Profile_Body : Buffer_Access := new Buffer_Type;

   begin

      --  A TAG_INTERNET_IOP Profile Body is an encapsulation.
      Start_Encapsulation (Profile_Body);

      --  Version
      Marshall (Profile_Body, Types.Octet (IIOP_Major_Version));
      Marshall (Profile_Body, Types.Octet (IIOP_Minor_Version));

      --  Marshalling of a Socket
      Marshall_Socket (Profile_Body, IIOP_Profile.Address);

      --  Marshalling of the Object Id
      Marshall
        (Profile_Body, Stream_Element_Array
         (IIOP_Profile.Object_Id.all));

      --  Marshall the tagged components (none for now).
      Marshall (Profile_Body, Types.Unsigned_Long'(0));

      --  Marshall the Profile_Body into IOR.
      Marshall (Buf, Encapsulate (Profile_Body));
      Release (Profile_Body);

   end Marshall_IIOP_Profile_Body;


   ----------------------------------
   -- Unmarshall_IIOP_Profile_Body --
   ----------------------------------

   function Unmarshall_IIOP_Profile_Body
     (Buffer       : access Buffer_Type)
     return Profile_Access
   is
      Profile_Body   : aliased Encapsulation := Unmarshall (Buffer);
      Profile_Buffer : Buffer_Access := new Buffers.Buffer_Type;
      --  Length         : CORBA.Long;
      Result         : Profile_Access := new IIOP_Profile_Type;
      TResult        : IIOP_Profile_Type
        renames IIOP_Profile_Type (Result.all);


   begin
      Decapsulate (Profile_Body'Access, Profile_Buffer);

      TResult.Major_Version  := Unmarshall (Profile_Buffer);
      TResult.Minor_Version  := Unmarshall (Profile_Buffer);

      Unmarshall_Socket (Profile_Buffer, TResult.Address);

      declare
         Str  : aliased Stream_Element_Array :=
           Unmarshall (Profile_Buffer);
      begin
         TResult.Object_Id := new Object_Id'(Object_Id (Str));
         if TResult.Minor_Version /= 0 then
            TResult.Components := Unmarshall_Tagged_Component
              (Profile_Buffer);
         end if;
      end;
      Release (Profile_Buffer);
      return Result;

   end Unmarshall_IIOP_Profile_Body;

   procedure Marshall_Socket
       (Buffer   : access Buffer_Type;
        Sock     : Sockets.Sock_Addr_Type)

   is
      Str  : Types.String := To_Droopi_String (Image (Sock.Addr));
   begin

      --  Marshalling of the Host as a string
      Marshall (Buffer, Str);

      --  Marshalling of the port
      Marshall (Buffer, Types.Unsigned_Short (Sock.Port));

   end Marshall_Socket;


   procedure Unmarshall_Socket
    (Buffer   : access Buffer_Type;
     Sock     : out Sockets.Sock_Addr_Type)

   is
      Str  : Types.String := Unmarshall (Buffer);
      Port : Types.Unsigned_Short;
   begin

      --  Unmarshalling of the Host
      Sock.Addr := Inet_Addr (To_Standard_String (Str));

      --  Unmarshalling of the port
      Port := Unmarshall (Buffer);
      Sock.Port := Port_Type (Port);

   end Unmarshall_Socket;


   procedure Marshall_Tagged_Component
     (Buffer         : access Buffer_Type;
      Components     : Component_Seq.Sequence)

   is
      use Component_Seq;
   begin

      Marshall (Buffer,  Types.Unsigned_Long (Length (Components)));
      for I in 1 .. Length (Components) loop
         Marshall (Buffer, Element_Of (Components, I).Tag);
         Marshall (Buffer, Element_Of (Components, I).Component_Data.all);
      end loop;

   end Marshall_Tagged_Component;


   function  Unmarshall_Tagged_Component
     (Buffer   : access Buffer_Type)
     return Component_Seq.Sequence
   is
      use Component_Seq;
      Comp        : Tagged_Component;
      Components  : Component_Seq.Sequence := Null_Sequence;
      Len         : Types.Unsigned_Long;
   begin
      Len := Unmarshall (Buffer);
      for I in 1 .. Len loop
         Comp.Tag  := Unmarshall (Buffer);
         Comp.Component_Data := new Stream_Element_Array'(Unmarshall
                            (Buffer));
         Append (Components, Comp);
      end loop;
      return Components;
   end Unmarshall_Tagged_Component;

   -----------
   -- Image --
   -----------

   function Image (Prof : IIOP_Profile_Type) return String is
   begin
      return "Address : " & Image (Prof.Address) &
        ", Object_Id : " & Droopi.Objects.Image (Prof.Object_Id.all);
   end Image;

end Droopi.Binding_Data.IIOP;
