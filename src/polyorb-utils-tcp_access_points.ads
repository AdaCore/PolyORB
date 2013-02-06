------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . T C P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2013, Free Software Foundation, Inc.          --
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

--  Helper subprograms to set up access points based on TCP sockets
--  for a PolyORB server.

with PolyORB.Sockets;
with PolyORB.Transport;
with PolyORB.Utils.Socket_Access_Points;

package PolyORB.Utils.TCP_Access_Points is

   use PolyORB.Sockets;
   use PolyORB.Transport;
   use PolyORB.Utils.Socket_Access_Points;

   ----------------------------------
   -- Access_Point_Info descriptor --
   ----------------------------------

   procedure Initialize_Socket
     (SAP       : out Transport_Access_Point_Access;
      Address   : Sockets.Inet_Addr_Type := Any_Inet_Addr;
      Port_Hint : Port_Interval;
      Publish   : String := "");
   --  Initialize API.Socket and bind it to a free port, using one of
   --  the address corresponding to hostname, or use Address, and within
   --  the range given by Port_Hint if applicable (if Port_Hint.Lo is
   --  Any_Port, then Port_Hint.Hi is ignored).

   type AP_Infos is array (Positive range <>) of Transport_Access_Point_Access;
   function Initialize_Access_Points
     (Listen_Spec : String;
      Default_Ports : Port_Interval := (Any_Port, Any_Port)) return AP_Infos;
   pragma Unreferenced (Initialize_Access_Points);
   --  Needs documentation???
   --  Not used yet???

end PolyORB.Utils.TCP_Access_Points;
