------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             P O L Y O R B . B I N D I N G _ D A T A . S R P              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  Binding data for the Simple Request Protocol over TCP.

--  $Id$

with PolyORB.Sockets;

package PolyORB.Binding_Data.SRP is

   pragma Elaborate_Body;

   type SRP_Profile_Type is new Profile_Type with private;

   procedure Initialize (P : in out SRP_Profile_Type);
   procedure Adjust (P : in out SRP_Profile_Type);
   procedure Finalize (P : in out SRP_Profile_Type);

   procedure Bind_Non_Local_Profile
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
     (PF  : out SRP_Profile_Factory;
      TAP : Transport.Transport_Access_Point_Access;
      ORB : Components.Component_Access);

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
   end record;

   type SRP_Profile_Factory is new Profile_Factory
     with record
        Address : Sockets.Sock_Addr_Type;
     end record;

end PolyORB.Binding_Data.SRP;
