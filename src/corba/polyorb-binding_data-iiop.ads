------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . B I N D I N G _ D A T A . I I O P             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Example binding data concrete implementation.

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Buffers; use PolyORB.Buffers;
with PolyORB.Sequences.Unbounded;
with PolyORB.Sockets;
with PolyORB.Storage_Pools;
with PolyORB.Types;

package PolyORB.Binding_Data.IIOP is

   pragma Elaborate_Body;

   type IIOP_Profile_Type is new Profile_Type with private;

   type Octets_Access is access all Stream_Element_Array;
   for Octets_Access'Storage_Pool use PolyORB.Storage_Pools.Debug_Pool;

   type Tagged_Component is record
      Tag            : Types.Unsigned_Long;
      Component_Data : Octets_Access;
   end record;

   package Component_Seq is new PolyORB.Sequences.Unbounded (Tagged_Component);

   procedure Initialize;

   procedure Initialize (P : in out IIOP_Profile_Type);
   procedure Adjust     (P : in out IIOP_Profile_Type);
   procedure Finalize   (P : in out IIOP_Profile_Type);

   function Bind_Profile
     (Profile : IIOP_Profile_Type;
      The_ORB : Components.Component_Access)
     return Components.Component_Access;

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
     (PF  : out IIOP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access);

   function Create_Profile
     (PF  : access IIOP_Profile_Factory;
      Oid :        Objects.Object_Id)
     return Profile_Access;

   function Is_Local_Profile
     (PF : access IIOP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean;

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
      Components : Component_Seq.Sequence;
   end record;

   type IIOP_Profile_Factory is new Profile_Factory with record
      Address : Sockets.Sock_Addr_Type;
   end record;

end PolyORB.Binding_Data.IIOP;
