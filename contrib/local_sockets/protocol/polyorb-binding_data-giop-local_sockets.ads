------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.BINDING_DATA.GIOP.LOCAL_SOCKETS                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with PolyORB.Buffers;
with PolyORB.Types;

with PolyORB.Local_Sockets;

package PolyORB.Binding_Data.GIOP.Local_Sockets is

   use PolyORB.Buffers;

   type LS_Profile_Type is new GIOP_Profile_Type with private;
   type LS_Profile_Factory is new GIOP_Profile_Factory with private;

   function Create_Profile
     (PF   : access LS_Profile_Factory;
      Oid  : Objects.Object_Id)
      return Profile_Access;

   function Duplicate_Profile (P : LS_Profile_Type) return Profile_Access;

   function Is_Local_Profile
     (PF   : access LS_Profile_Factory;
      P    : access Profile_Type'Class)
      return Boolean;

   procedure Bind_Profile
     (Profile : LS_Profile_Type;
      The_ORB : Components.Component_Access;
      BO_Ref  : out Smart_Pointers.Ref;
      Error   : out Errors.Error_Container);

   function Get_Profile_Tag (Profile : LS_Profile_Type) return Profile_Tag;
   pragma Inline (Get_Profile_Tag);

   function Get_Profile_Preference
     (Profile : LS_Profile_Type)
      return    Profile_Preference;
   pragma Inline (Get_Profile_Preference);

   procedure Create_Factory
     (PF  : out LS_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access);

   procedure Marshall_LS_Profile_Body
     (Buf     : access Buffer_Type;
      Profile : Profile_Access);

   function Unmarshall_LS_Profile_Body
     (Buffer : access Buffer_Type)
      return   Profile_Access;

   function Image (Prof : LS_Profile_Type) return String;

   function Image (Prof : LS_Profile_Factory) return String;

   function Get_OA
     (Profile : LS_Profile_Type)
      return    PolyORB.Smart_Pointers.Entity_Ptr;
   pragma Inline (Get_OA);

private
   use PolyORB.Local_Sockets;

   LS_Version_Major : constant Types.Octet := 1;
   LS_Version_Minor : constant Types.Octet := 0;

   type LS_Profile_Type is new GIOP_Profile_Type with record
      --  Socket information

      Address : Local_Socket_Addr;
   end record;

   type LS_Profile_Factory is new GIOP_Profile_Factory with record
      Address : Local_Socket_Addr;
   end record;

end PolyORB.Binding_Data.GIOP.Local_Sockets;
