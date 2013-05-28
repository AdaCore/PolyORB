------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . T C P _ A C C E S S _ P O I N T S       --
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

--  Utility routines to set up TCP listening sockets

with Ada.Exceptions;

with PolyORB.Components;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Sockets;

package body PolyORB.Utils.TCP_Access_Points is

   use PolyORB.Log;
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Utils.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.utils.tcp_access_points");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   --  function C (Level : Log_Level := Debug) return Boolean
   --    renames L.Enabled;

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (API       : in out Access_Point_Info;
      Address   : PolyORB.Sockets.Inet_Addr_Type := Any_Inet_Addr;
      Port_Hint : Port_Interval)
   is
   begin
      Utils.Sockets.Create_Socket (API.Socket);

      API.Address :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port_Hint.Lo,
                        Family => Family_Inet);

      --  Allow reuse of local addresses

      Set_Socket_Option (API.Socket, Socket_Level, (Reuse_Address, True));

      if API.SAP = null then
         API.SAP := new Socket_Access_Point;
      end if;

      loop
         begin
            Create
              (Socket_Access_Point (API.SAP.all),
               API.Socket,
               API.Address);
            exit;
         exception
            when E : PolyORB.Sockets.Socket_Error =>

               --  If a specific port range was given, try next port in range

               if API.Address.Port /= Any_Port
                 and then API.Address.Port < Port_Hint.Hi
               then
                  API.Address.Port := API.Address.Port + 1;
               else
                  O ("bind failed: " & Ada.Exceptions.Exception_Message (E),
                     Notice);
                  raise;
               end if;

         end;
      end loop;

      --  Create profile factory

      if API.PF /= null then
         Create_Factory
           (API.PF.all,
            API.SAP,
            Components.Component_Access (Setup.The_ORB));
      end if;
   end Initialize_Socket;

end PolyORB.Utils.TCP_Access_Points;
