------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . B I N D I N G _ D A T A . G I O P . I I O P        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Binding data concrete implementation for IIOP.

with PolyORB.Buffers;
with PolyORB.Sockets;
with PolyORB.Types;

package PolyORB.Binding_Data.GIOP.IIOP is

   use PolyORB.Buffers;

   type IIOP_Profile_Type is new GIOP_Profile_Type with private;
   type IIOP_Profile_Factory is new GIOP_Profile_Factory with private;

   function Create_Profile
     (PF  : access IIOP_Profile_Factory;
      Oid :        Objects.Object_Id)
      return Profile_Access;

   procedure Bind_Profile
     (Profile :     IIOP_Profile_Type;
      The_ORB :     Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container);

   function Is_Local_Profile
     (PF : access IIOP_Profile_Factory;
      P  : access Profile_Type'Class)
      return Boolean;

   function Get_Profile_Tag (Profile : IIOP_Profile_Type) return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : IIOP_Profile_Type)
     return Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   procedure Create_Factory
     (PF  : out IIOP_Profile_Factory;
      TAP :     Transport.Transport_Access_Point_Access;
      ORB :     Components.Component_Access);

   procedure Marshall_IIOP_Profile_Body
     (Buf     : access Buffer_Type;
      Profile :        Profile_Access);

   function Unmarshall_IIOP_Profile_Body
     (Buffer : access Buffer_Type)
     return Profile_Access;

   function Image (Prof : IIOP_Profile_Type) return String;

   function Get_OA
     (Profile : IIOP_Profile_Type)
     return PolyORB.Smart_Pointers.Entity_Ptr;
   pragma Inline (Get_OA);

private

   IIOP_Version_Major : constant Types.Octet := 1;
   IIOP_Version_Minor : constant Types.Octet := 2;

   type IIOP_Profile_Type is new GIOP_Profile_Type with record

      --  Socket information

      Address       : Sockets.Sock_Addr_Type;
   end record;

   type IIOP_Profile_Factory is new GIOP_Profile_Factory with record
      Address : Sockets.Sock_Addr_Type;
   end record;

end PolyORB.Binding_Data.GIOP.IIOP;
