------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S O C K E T S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

--  $Id$

--  General purpose functions for using sockets with string and buffers

with PolyORB.Buffers;
with PolyORB.Types;
with PolyORB.Sockets;

package PolyORB.Utils.Sockets is

   function String_To_Addr
     (Str : Types.String)
     return PolyORB.Sockets.Inet_Addr_Type;
   --  Convert a string to an inet address
   --  String can be a numerical IP (eg: 127.0.0.1)
   --  or a string addess (eg: localhost)

   procedure Marshall_Socket
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Sock   : in     PolyORB.Sockets.Sock_Addr_Type);
   --  Marshall socket address and port in a buffer

   procedure Unmarshall_Socket
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Sock   :    out PolyORB.Sockets.Sock_Addr_Type);
   --  Unmarshall socket address and port from a buffer

end PolyORB.Utils.Sockets;
