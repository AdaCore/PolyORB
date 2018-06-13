------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . T C P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2017, Free Software Foundation, Inc.          --
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

--  Utility routines to set up TCP listening sockets

with Ada.Exceptions;

with PolyORB.Log;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Sockets;

package body PolyORB.Utils.TCP_Access_Points is

   use PolyORB.Log;
   use PolyORB.Transport.Connected.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.utils.tcp_access_points");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   function Initialize_Access_Points
     (Listen_Spec   : String;
      Default_Ports : Port_Interval := (Any_Port, Any_Port)) return APs
   is
      function Initialize_TCP_AP
        (Addr      : Inet_Addr_Type;
         Port_Hint : Port_Interval) return Transport_Access_Point_Access;
      --  Initialize a connected socket access point

      function Initialize_TCP_AP
        (Addr      : Inet_Addr_Type;
         Port_Hint : Port_Interval) return Transport_Access_Point_Access
      is
      begin
         return TAP : Transport_Access_Point_Access do
            Initialize_Socket (TAP, Addr, Port_Hint);
         end return;
      end Initialize_TCP_AP;

   --  Start of processing for Initialize_Access_Points

   begin
      return Initialize_Access_Points
        (Listen_Spec, Default_Ports, Initialize_TCP_AP'Access);
   end Initialize_Access_Points;

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (SAP       : out Transport_Access_Point_Access;
      Address   : PolyORB.Sockets.Inet_Addr_Type := Any_Inet_Addr;
      Port_Hint : Port_Interval)
   is
      Socket  : Socket_Type;
      S_Addr  : Sock_Addr_Type;
   begin
      Utils.Sockets.Create_Socket
        (Socket,
         Mode          => Socket_Stream,
         Reuse_Address => True);

      S_Addr :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port_Hint.Lo,
                        Family => Family_Inet);

      --  Allow reuse of local addresses

      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));

      SAP := new Connected_Socket_AP;

      loop
         begin
            Create
              (Connected_Socket_AP (SAP.all),
               Socket,
               S_Addr);
            exit;
         exception
            when E : PolyORB.Sockets.Socket_Error =>

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

      pragma Debug (C, O ("Created TCP AP: "
         & Image (Socket_AP_Address (Connected_Socket_AP (SAP.all)))));
   end Initialize_Socket;

end PolyORB.Utils.TCP_Access_Points;
