------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . U D P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
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

--  Helper subprograms to set up access points based on UDP sockets

with Ada.Exceptions;

with PolyORB.Log;
with PolyORB.Platform;
with PolyORB.Transport.Datagram.Sockets;
with PolyORB.Utils.Sockets;

package body PolyORB.Utils.UDP_Access_Points is

   use PolyORB.Log;
   use PolyORB.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.utils.udp_access_points");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   --  function C (Level : Log_Level := Debug) return Boolean
   --    renames L.Enabled;

   procedure Initialize_Socket (Socket : in out Socket_Type);
   pragma Inline (Initialize_Socket);
   --  Shared part between Initialize_Unicast_Socket and
   --  Initialize_Multicast_Socket.

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket (Socket : in out Socket_Type) is
   begin
      Utils.Sockets.Create_Socket (Socket, Family_Inet, Socket_Datagram);

      --  Allow reuse of local addresses

      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
   end Initialize_Socket;

   ---------------------------------
   -- Initialize_Multicast_Socket --
   ---------------------------------

   procedure Initialize_Multicast_Socket
     (SAP     : out Transport.Transport_Access_Point_Access;
      Address : Inet_Addr_Type;
      Port    : Port_Type)
   is
      use PolyORB.Transport.Datagram.Sockets;

      Socket : Socket_Type;

      S_Addr : Sock_Addr_Type;
      --  Multicast group address

      B_Addr : Sock_Addr_Type;
      --  Address to which the socket must be bound (see platform comments
      --  below).
   begin
      SAP := new Socket_Access_Point;

      Initialize_Socket (Socket);

      S_Addr :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port,
                        Family => Family_Inet);

      B_Addr := S_Addr;

      --  Bind socket: for UNIX it needs to be bound to the group address;
      --  for Windows to INADDR_ANY.

      if PolyORB.Platform.Windows_On_Target then
         B_Addr.Addr := Any_Inet_Addr;
      end if;

      Init_Socket
        (Socket_Access_Point (SAP.all),
         Socket,
         Address      => S_Addr,
         Bind_Address => B_Addr,
         Update_Addr  => False);

      --  Join multicast group on the appropriate interface (note that under
      --  Windows, this is possible only after the socket is bound).

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Name              => Add_Membership,
          Multicast_Address => Address,
          Local_Interface   => Any_Inet_Addr));

      --  Allow local multicast operation

      Set_Socket_Option
        (Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_Loop, True));
   end Initialize_Multicast_Socket;

   -------------------------------
   -- Initialize_Unicast_Socket --
   -------------------------------

   procedure Initialize_Unicast_Socket
     (SAP       : out Transport.Transport_Access_Point_Access;
      Port_Hint : Port_Interval;
      Address   : Inet_Addr_Type := Any_Inet_Addr)
   is
      use PolyORB.Transport.Datagram.Sockets;
      Socket : Socket_Type;
      S_Addr : Sock_Addr_Type;
   begin
      SAP := new Socket_Access_Point;

      --  Create Socket

      Initialize_Socket (Socket);

      S_Addr :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port_Hint.Lo,
                        Family => Family_Inet);

      loop
         begin
            Init_Socket
              (Socket_Access_Point (SAP.all),
               Socket,
               S_Addr);
            exit;
         exception
            when E : Socket_Error =>

               --  If a specific port range was given, try next port in range

               if S_Addr.Port /= Any_Port
                 and then S_Addr.Port < Port_Hint.Hi
               then
                  S_Addr.Port := S_Addr.Port + 1;
               else
                  O ("bind failed: " & Ada.Exceptions.Exception_Message (E),
                     Notice);
                  raise;
               end if;
         end;
      end loop;
   end Initialize_Unicast_Socket;

end PolyORB.Utils.UDP_Access_Points;
