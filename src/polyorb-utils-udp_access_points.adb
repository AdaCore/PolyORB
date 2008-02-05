------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . U D P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
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

--  Helper subprograms to set up access points based on UDP sockets

with Ada.Exceptions;

with PolyORB.Components;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Transport.Datagram.Sockets_In;

package body PolyORB.Utils.UDP_Access_Points is

   use PolyORB.Binding_Data;
   use PolyORB.Log;
   use PolyORB.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.utils.udp_access_points");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   --  function C (Level : Log_Level := Debug) return Boolean
   --    renames L.Enabled;

   procedure Initialize_Socket (API : in out UDP_Access_Point_Info);
   pragma Inline (Initialize_Socket);
   --  Shared part between Initialize_Unicast_Socket and
   --  Initialize_Multicast_Socket.

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket (API : in out UDP_Access_Point_Info) is
   begin
      Create_Socket (API.Socket, Family_Inet, Socket_Datagram);

      --  Allow reuse of local addresses

      Set_Socket_Option
        (API.Socket,
         Socket_Level,
         (Reuse_Address, True));
   end Initialize_Socket;

   ---------------------------------
   -- Initialize_Multicast_Socket --
   ---------------------------------

   procedure Initialize_Multicast_Socket
     (API     : in out UDP_Access_Point_Info;
      Address : Inet_Addr_Type;
      Port    : Port_Type)
   is
      use PolyORB.Transport.Datagram.Sockets_In;

   begin
      Initialize_Socket (API);

      API.Address :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port,
                        Family => Family_Inet);

      --  Bind socket

      Init_Socket_In
        (Socket_In_Access_Point (API.SAP.all), API.Socket, API.Address, False);

      --  Register to multicast group (note that under Windows it is not
      --  possible to do so before the socket is bound).

      Set_Socket_Option
        (API.Socket,
         IP_Protocol_For_IP_Level,
         (Add_Membership, Address, Any_Inet_Addr));

      --  Allow local multicast operation

      Set_Socket_Option
        (API.Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_Loop, True));

      if API.PF /= null then
         Create_Factory
           (API.PF.all,
            API.SAP,
            PolyORB.Components.Component_Access (Setup.The_ORB));
      end if;
   end Initialize_Multicast_Socket;

   -------------------------------
   -- Initialize_Unicast_Socket --
   -------------------------------

   procedure Initialize_Unicast_Socket
     (API       : in out UDP_Access_Point_Info;
      Port_Hint : Port_Interval;
      Address   : Inet_Addr_Type := Any_Inet_Addr)
   is
      use PolyORB.Transport.Datagram.Sockets_In;

   begin
      --  Create Socket

      Initialize_Socket (API);

      API.Address :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port_Hint.Lo,
                        Family => Family_Inet);

      loop
         begin
            Init_Socket_In
              (Socket_In_Access_Point (API.SAP.all),
               API.Socket,
               API.Address);
            exit;
         exception
            when E : Sockets.Socket_Error =>

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
   end Initialize_Unicast_Socket;

end PolyORB.Utils.UDP_Access_Points;
