------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S O C K E T S                 --
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

--  General purpose functions for using sockets with string and buffers

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Buffers;
with PolyORB.Sockets;

package PolyORB.Utils.Sockets is

   use PolyORB.Sockets;

   type Socket_Name (Name_Len : Natural) is record
      Host_Name : String (1 .. Name_Len);
      Port      : Port_Type;
   end record;

   function To_Address
     (SN : Socket_Name) return Sock_Addr_Type;
   --  Convert socket name to socket address

   type Socket_Name_Ptr is access all Socket_Name;
   procedure Free is
     new PolyORB.Utils.Unchecked_Deallocation.Free

     (Object => Socket_Name,

      Name   => Socket_Name_Ptr);

   function "+"
     (Host_Name : String;
      Port      : Port_Type) return Socket_Name;
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
     (Sock        : in out Socket_Type;
      Remote_Name : Socket_Name);
   --  Wrapper for PolyORB.Sockets.Connect_Socket. In case of failure, Sock is
   --  closed, and a log trace is produced.

   function Is_IP_Address (Name : String) return Boolean;
   --  True iff S is an IP address in dotted quad notation

   function Local_Inet_Address return Inet_Addr_Type;
   --  Return an IP address associated with the local host name, preferring
   --  non-loopback addresses over loopback ones.

   procedure Create_Socket
     (Socket        : out Socket_Type;
      Family        : Family_Type := Family_Inet;
      Mode          : Mode_Type   := Socket_Stream;
      Reuse_Address : Boolean     := False);
   --  Wrapper for PolyORB.Sockets.Create_Socket that also marks the
   --  newly-created socket as close-on-exec. If Reuse_Address is true,
   --  the option is set on the socket.

   procedure Set_Close_On_Exec (Socket : Socket_Type);
   --  Mark S as not to be inherited by child processes

end PolyORB.Utils.Sockets;
