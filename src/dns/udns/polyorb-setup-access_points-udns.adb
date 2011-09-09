------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . U D N S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2011, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

--  Setup socket for UDNS

with PolyORB.Binding_Data.DNS.UDNS;
with PolyORB.Filters;

with PolyORB.Initialization;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.DNS;
with PolyORB.Sockets;
with PolyORB.Transport.Datagram.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.UDP_Access_Points;

package body PolyORB.Setup.Access_Points.UDNS is

   use PolyORB.Filters;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport.Datagram.Sockets;
   use PolyORB.Utils.UDP_Access_Points;
   use PolyORB.Utils.Socket_Access_Points;
   UDNS_Access_Point : UDP_Access_Point_Info
     := (Socket        => No_Socket,
         Address       => No_Sock_Addr,
         SAP           => new Socket_Access_Point,
         PF            =>
           new PolyORB.Binding_Data.DNS.UDNS.UDNS_Profile_Factory);

   Pro : aliased Protocols.DNS.DNS_Protocol;
   UDNS_Factories : aliased Filters.Factory_Array := (0 => Pro'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Parameters;

--        Addr : constant String :=
--                 Get_Conf ("udns", "polyorb.udns.multicast_addr", "");
--        Port : constant Port_Type :=
--         Port_Type (Get_Conf ("udns", "polyorb.udns.multicast_port", 0));
      Addr : constant Inet_Addr_Type :=
        Inet_Addr (String'(
          Get_Conf ("udns",
                       "polyorb.udns.unicast_addr", Image (No_Inet_Addr))));
--        Port : constant Port_Type :=
--          Port_Type (Get_Conf ("udns", "polyorb.udns.unicast_port", 0));
      Port_Hint : constant Port_Interval := To_Port_Interval
                          (Get_Conf
                           ("udns",
                            "polyorb.udns.unicast_port",
                            (Integer (Any_Port), Integer (Any_Port))));

   begin

      if Get_Conf ("access_points", "udns", True) then
--           Initialize_Multicast_Socket
--             (UDNS_Access_Point, Inet_Addr (Addr), Port);
         Initialize_Unicast_Socket (UDNS_Access_Point, Port_Hint, Addr);
         Register_Access_Point
           (ORB   => The_ORB,
            TAP   => UDNS_Access_Point.SAP,
            Chain => UDNS_Factories'Access,
            PF    => UDNS_Access_Point.PF);
      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"access_points.udns",
       Conflicts => String_Lists.Empty,
       Depends   => +"orb" & "sockets",
       Provides  => String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize_Access_Points'Access,
       Shutdown  => null));
end PolyORB.Setup.Access_Points.UDNS;
