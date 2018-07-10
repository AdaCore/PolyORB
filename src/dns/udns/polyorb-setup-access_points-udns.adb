------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . U D N S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2013, Free Software Foundation, Inc.          --
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

--  Setup socket for UDNS

pragma Ada_2012;

with PolyORB.Binding_Data.DNS.UDNS;
with PolyORB.Filters;

with PolyORB.Initialization;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.DNS;
with PolyORB.Sockets;
with PolyORB.Transport;
with PolyORB.Utils.Strings;
with PolyORB.Utils.UDP_Access_Points;

package body PolyORB.Setup.Access_Points.UDNS is

   use PolyORB.Filters;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Utils.UDP_Access_Points;
   use PolyORB.Utils.Socket_Access_Points;

   UDNS_Access_Point : Transport.Transport_Access_Point_Access;

   Pro : aliased Protocols.DNS.DNS_Protocol;
   UDNS_Factories : aliased Filters.Factory_Array := (0 => Pro'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use Parameters;
      use Binding_Data.DNS.UDNS;

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
         Initialize_Unicast_Socket (UDNS_Access_Point, Port_Hint, Addr);
         Register_Access_Point
           (ORB   => The_ORB,
            TAP   => UDNS_Access_Point,
            Chain => UDNS_Factories'Access,
            PF    => new UDNS_Profile_Factory'
                       (Create_Factory (UDNS_Access_Point)));
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
