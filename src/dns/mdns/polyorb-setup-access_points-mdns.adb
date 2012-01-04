------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . M D N S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Setup socket for MDNS

with PolyORB.Binding_Data.DNS.MDNS;
with PolyORB.Filters;

with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.DNS;
with PolyORB.Sockets;
with PolyORB.Transport.Datagram.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.UDP_Access_Points;

package body PolyORB.Setup.Access_Points.MDNS is

   use PolyORB.Filters;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport.Datagram.Sockets;
   use PolyORB.Utils.UDP_Access_Points;

   MDNS_Access_Point : UDP_Access_Point_Info
     := (Socket        => No_Socket,
         Address       => No_Sock_Addr,
         SAP           => new Socket_Access_Point,
         PF            =>
           new PolyORB.Binding_Data.DNS.MDNS.MDNS_Profile_Factory);

   Pro : aliased Protocols.DNS.DNS_Protocol;
   MDNS_Factories : aliased Filters.Factory_Array := (0 => Pro'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Parameters;

      Addr : constant String :=
               Get_Conf ("mdns", "polyorb.mdns.multicast_addr", "");
      Port : constant Port_Type :=
               Port_Type (Get_Conf ("mdns", "polyorb.mdns.multicast_port", 0));
   begin
      if Get_Conf ("access_points", "mdns", True) then

         --  If multicast group address or port number is not set, access point
         --  is deactivated.

         if Addr = "" or else Port = 0 then
            return;
         end if;

         Initialize_Multicast_Socket
           (MDNS_Access_Point, Inet_Addr (Addr), Port);

         Register_Access_Point
           (ORB   => The_ORB,
            TAP   => MDNS_Access_Point.SAP,
            Chain => MDNS_Factories'Access,
            PF    => null);

         --  Note: the mDNS access point has a null profile factory chain,
         --  so no profile designating it is ever assigned automatically to
         --  objects. References designating the mDNS multicast group must be
         --  created manually.

      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"access_points.mdns",
       Conflicts => String_Lists.Empty,
       Depends   => +"orb" & "sockets",
       Provides  => String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize_Access_Points'Access,
       Shutdown  => null));
end PolyORB.Setup.Access_Points.MDNS;
