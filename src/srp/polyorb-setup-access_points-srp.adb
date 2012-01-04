------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . S R P       --
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

--  Setup for SRP access point.

with PolyORB.Binding_Data.SRP;
with PolyORB.Protocols.SRP;

with PolyORB.Filters;
with PolyORB.Initialization;

with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols;
with PolyORB.Sockets;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.TCP_Access_Points;

package body PolyORB.Setup.Access_Points.SRP is

   use PolyORB.Filters;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Utils.TCP_Access_Points;

   --  The 'SRP' access point.

   SRP_Access_Point : Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.SRP.SRP_Profile_Factory);

   SRP_Protocol  : aliased Protocols.SRP.SRP_Protocol;
   SRP_Factories : aliased Filters.Factory_Array
     := (0 => SRP_Protocol'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Parameters;
   begin
      if Get_Conf ("access_points", "srp", True) then

         Initialize_Socket
           (SRP_Access_Point, Any_Inet_Addr, (Any_Port, Any_Port));

         Register_Access_Point
           (ORB    => The_ORB,
            TAP    => SRP_Access_Point.SAP,
            Chain  => SRP_Factories'Access,
            PF     => SRP_Access_Point.PF);
         --  Register socket with ORB object, associating a protocol
         --  to the transport service access point.
      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"access_points.srp",
       Conflicts => Empty,
       Depends   => +"orb",
       Provides  => +"access_points",
       Implicit  => False,
       Init      => Initialize_Access_Points'Access,
       Shutdown  => null));
end PolyORB.Setup.Access_Points.SRP;
