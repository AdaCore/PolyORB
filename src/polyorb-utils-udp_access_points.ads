------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E T U P . U D P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                             . S O C K E T S                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2002 Free Software Foundation, Inc.           --
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

--  Helper subprograms to set up access points based on UDP sockets

--  $Id$

with PolyORB.Binding_Data;
with PolyORB.Sockets;
with PolyORB.Transport;

package PolyORB.Utils.UDP_Access_Points is

   type UDP_Access_Point_Info is record
      Socket  : Sockets.Socket_Type;
      Address : Sockets.Sock_Addr_Type;

      SAP     : Transport.Transport_Access_Point_Access;
      PF      : Binding_Data.Profile_Factory_Access;
   end record;

   procedure Initialize_Unicast_Socket
     (API       : in out UDP_Access_Point_Info;
      Port_Hint : in     Sockets.Port_Type);

   procedure Initialize_Multicast_Socket
     (API     : in out UDP_Access_Point_Info;
      Address : in     Sockets.Inet_Addr_Type;
      Port    : in     Sockets.Port_Type);

end PolyORB.Utils.UDP_Access_Points;
