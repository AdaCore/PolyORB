------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E T U P . T C P _ A C C E S S _ P O I N T S       --
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

--  Helper subprograms to set up access points based on TCP sockets
--  for a PolyORB server.

--  $Id$

with PolyORB.Binding_Data;
with PolyORB.Sockets;
with PolyORB.Transport;

package PolyORB.Setup.TCP_Access_Points is

   use PolyORB.Binding_Data;
   use PolyORB.Sockets;
   use PolyORB.Transport;

   ----------------------------------
   -- Access_Point_Info descriptor --
   ----------------------------------

   type Access_Point_Info is record
      Socket  : Socket_Type;
      Address : Sock_Addr_Type;

      SAP : Transport_Access_Point_Access;
      PF  : Profile_Factory_Access;
   end record;

   procedure Initialize_Socket
     (DAP  : in out Access_Point_Info;
      Port : Port_Type);
   --  Initialize DAP.Socket and bind it to a free port,
   --  Port if possible.

end PolyORB.Setup.TCP_Access_Points;
