------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S O C K E T S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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

--  General purpose functions for using sockets with string and buffers

with Ada.Unchecked_Deallocation;

with PolyORB.Buffers;
with PolyORB.Sockets;

package PolyORB.Utils.Sockets is

   function String_To_Addr
     (Str : Standard.String) return PolyORB.Sockets.Inet_Addr_Type;
   --  Convert an IP address in dotted decimal form or a host name into an
   --  Inet_Addr_Type value.

   type Socket_Name (Name_Len : Natural) is record
      Host_Name : String (1 .. Name_Len);
      Port      : PolyORB.Sockets.Port_Type;
   end record;

   type Socket_Name_Ptr is access all Socket_Name;
   procedure Free is
     new Ada.Unchecked_Deallocation (Socket_Name, Socket_Name_Ptr);

   function "+"
     (Host_Name : String;
      Port      : PolyORB.Sockets.Port_Type) return Socket_Name;
   --  Return a Socket_Name with the given contents

   function Image (SN : Socket_Name) return String;
   --  Return representation of SN as <host_name>:<port>

   procedure Marshall_Socket
     (Buffer : access PolyORB.Buffers.Buffer_Type;
      Sock   : Socket_Name);
   --  Marshall socket address and port in a buffer

   function Unmarshall_Socket
     (Buffer : access PolyORB.Buffers.Buffer_Type) return Socket_Name;
   --  Unmarshall socket address and port from a buffer

   procedure Connect_Socket
     (Sock        : PolyORB.Sockets.Socket_Type;
      Remote_Name : Socket_Name);
   --  Front-end to PolyORB.Sockets.Connect_Socket, handles production of log
   --  trace if the operation fails.

   function Is_IP_Address (Name : String) return Boolean;
   --  True iff S is an IP address in dotted quad notation

end PolyORB.Utils.Sockets;
