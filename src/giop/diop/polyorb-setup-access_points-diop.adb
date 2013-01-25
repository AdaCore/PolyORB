------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . D I O P      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  Setup socket for DIOP

with PolyORB.Binding_Data.GIOP.DIOP;
with PolyORB.Protocols.GIOP.DIOP;

with PolyORB.Filters;
with PolyORB.Filters.Fragmenter;
with PolyORB.Initialization;

with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols;
with PolyORB.Sockets;
with PolyORB.Transport.Datagram.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.Utils.UDP_Access_Points;

package body PolyORB.Setup.Access_Points.DIOP is

   use PolyORB.Filters;
   use PolyORB.Filters.Fragmenter;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport.Datagram.Sockets;
   use PolyORB.Utils.Socket_Access_Points;
   use PolyORB.Utils.UDP_Access_Points;

   DIOP_Access_Point : UDP_Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new PolyORB.Binding_Data.GIOP.DIOP.DIOP_Profile_Factory);

   Fra : aliased Fragmenter_Factory;
   Pro : aliased Protocols.GIOP.DIOP.DIOP_Protocol;
   DIOP_Factories : aliased Filters.Factory_Array
     := (0 => Fra'Access, 1 => Pro'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Parameters;

   begin
      if Get_Conf ("access_points", "diop", True) then
         declare
            Port_Hint : constant Port_Interval := To_Port_Interval
                          (Get_Conf
                           ("diop",
                            "polyorb.protocols.diop.default_port",
                            (Integer (Any_Port), Integer (Any_Port))));

            Addr : constant Inet_Addr_Type :=
              Inet_Addr (String'(Get_Conf
                                   ("diop",
                                    "polyorb.protocols.diop.default_addr",
                                    Image (No_Inet_Addr))));
         begin
            Initialize_Unicast_Socket (DIOP_Access_Point, Port_Hint, Addr);

            Register_Access_Point
              (ORB    => The_ORB,
               TAP    => DIOP_Access_Point.SAP,
               Chain  => DIOP_Factories'Access,
               PF     => DIOP_Access_Point.PF);
         end;
      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"access_points.diop",
       Conflicts => String_Lists.Empty,
       Depends   => +"orb" & "sockets",
       Provides  => String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize_Access_Points'Access,
       Shutdown  => null));
end PolyORB.Setup.Access_Points.DIOP;
