------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
-- P O L Y O R B . S E T U P . U D P _ A C C E S S _ P O I N T S . D I O P  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Setup socket for DIOP

with PolyORB.Binding_Data.DIOP;
with PolyORB.Protocols.GIOP.DIOP;

with PolyORB.Configuration;
with PolyORB.Filters;
with PolyORB.Filters.Fragmenter;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Transport.Datagram.Sockets_In;
with PolyORB.Utils.Strings;

package body PolyORB.Setup.UDP_Access_Points.Sockets.DIOP is

   use PolyORB.Filters;
   use PolyORB.Filters.Fragmenter;
   use PolyORB.ORB;
   use PolyORB.Transport.Datagram.Sockets_In;

   DIOP_Access_Point : UDP_Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_In_Access_Point,
         PF      => new Binding_Data.DIOP.DIOP_Profile_Factory);

   Pro  : aliased Protocols.GIOP.DIOP.DIOP_Protocol;
   Frag : aliased Fragmenter_Factory;

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Configuration;

   begin
      if Get_Conf ("access_points", "diop", True) then
         Initialize_Socket (DIOP_Access_Point, Any_Port);

         Chain_Factories ((0 => Frag'Unchecked_Access,
                           1 => Pro'Unchecked_Access));

         Register_Access_Point
           (ORB    => The_ORB,
            TAP    => DIOP_Access_Point.SAP,
            Chain  => Frag'Unchecked_Access,
            PF     => DIOP_Access_Point.PF);
      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"udp_access_points.diop",
       Conflicts => String_Lists.Empty,
       Depends => +"protocols.giop",
       Provides => String_Lists.Empty,
       Init => Initialize_Access_Points'Access));

end PolyORB.Setup.UDP_Access_Points.Sockets.DIOP;
