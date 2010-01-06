------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . S S L _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

--  Helper subprograms to set up access points based on SSL sockets
--  for a PolyORB server.

with PolyORB.Binding_Data;
with PolyORB.Sockets;
with PolyORB.SSL;
with PolyORB.Transport;
with PolyORB.Utils.Socket_Access_Points;

package PolyORB.Utils.SSL_Access_Points is

   use PolyORB.Utils.Socket_Access_Points;

   ----------------------------------
   -- Access_Point_Info descriptor --
   ----------------------------------

   type Access_Point_Info is record
      Socket   : Sockets.Socket_Type;
      Address  : Sockets.Sock_Addr_Type;
      SAP      : Transport.Transport_Access_Point_Access;
      PF       : Binding_Data.Profile_Factory_Access;
   end record;

   procedure Initialize_Socket
     (API       : out Access_Point_Info;
      Address   : Sockets.Inet_Addr_Type;
      Port_Hint : Port_Interval;
      Context   : SSL.SSL_Context_Type);
   --  Initialize API.Socket and bind it to a free port, using one of
   --  the address corresponding to hostname, or use Address and
   --  Port_Hint if possible.

end PolyORB.Utils.SSL_Access_Points;
