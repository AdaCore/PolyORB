--  Example binding data concrete implementation.

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Buffers; use PolyORB.Buffers;
with PolyORB.Sockets;
with PolyORB.Storage_Pools;
with PolyORB.Types;

with Sequences.Unbounded;

package PolyORB.Binding_Data.IIOP is

   pragma Elaborate_Body;

   type IIOP_Profile_Type is new Profile_Type with private;

   type Octets_Access is access all Stream_Element_Array;
   for Octets_Access'Storage_Pool use PolyORB.Storage_Pools.Debug_Pool;

   type Tagged_Component is record
      Tag            : Types.Unsigned_Long;
      Component_Data : Octets_Access;
   end record;

   package Component_Seq is new Sequences.Unbounded (Tagged_Component);

   procedure Initialize;

   procedure Initialize (P : in out IIOP_Profile_Type);
   procedure Adjust     (P : in out IIOP_Profile_Type);
   procedure Finalize   (P : in out IIOP_Profile_Type);

   function Get_Object_Key
     (Profile : IIOP_Profile_Type)
     return Objects.Object_Id;
   --  XXX Change to return an OID_Access

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

   procedure Marshall_Tagged_Component
     (Buffer        : access Buffer_Type;
      Components    : Component_Seq.Sequence);

   function  Unmarshall_Tagged_Component
     (Buffer   : access Buffer_Type)
     return Component_Seq.Sequence;

   function Image (Prof : IIOP_Profile_Type) return String;

private

   IIOP_Major_Version : constant Types.Octet := 1;
   IIOP_Minor_Version : constant Types.Octet := 2;

   type IIOP_Profile_Type is new Profile_Type with record
      Major_Version : Types.Octet := IIOP_Major_Version;
      Minor_Version : Types.Octet := IIOP_Minor_Version;
      Address    : Sockets.Sock_Addr_Type;
      Object_Id  : Objects.Object_Id_Access;
      Components : Component_Seq.Sequence;
   end record;

   type IIOP_Profile_Factory is new Profile_Factory with record
      Address : Sockets.Sock_Addr_Type;
   end record;

end PolyORB.Binding_Data.IIOP;
