------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . U D P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Components;
with PolyORB.Setup;
with PolyORB.Transport.Datagram.Sockets_In;

package body PolyORB.Utils.UDP_Access_Points is

   use PolyORB.Binding_Data;
   use PolyORB.Sockets;

   procedure Initialize_Socket (API : in out UDP_Access_Point_Info);
   pragma Inline (Initialize_Socket);
   --  Shared part between Initialize_Unicast_Socket and
   --  Initialize_Multicast_Socket.

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket (API : in out UDP_Access_Point_Info) is
   begin
      Create_Socket
        (API.Socket, Family_Inet, Socket_Datagram);

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
        Sock_Addr_Type'(Addr => Address,
                        Port => Port,
                        Family => Family_Inet);

      Set_Socket_Option
        (API.Socket,
         IP_Protocol_For_IP_Level,
         (Add_Membership, Address, Any_Inet_Addr));
      --  Register to multicast group

      Set_Socket_Option
        (API.Socket,
         IP_Protocol_For_IP_Level,
         (Multicast_Loop, True));
      --  Allow local multicast operation

      Init_Socket_In
        (Socket_In_Access_Point (API.SAP.all), API.Socket, API.Address, False);

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
      Port_Hint : Port_Type;
      Address   : Inet_Addr_Type := Any_Inet_Addr)
   is
      use PolyORB.Transport.Datagram.Sockets_In;

   begin
      --  Create Socket

      Initialize_Socket (API);

      --  Find a free port, search begin at Port_Hint

      API.Address.Addr := Address;
      API.Address.Port := Port_Hint;
      loop
         begin
            Init_Socket_In
              (Socket_In_Access_Point (API.SAP.all),
               API.Socket,
               API.Address);
            exit;
         exception
            when PolyORB.Sockets.Socket_Error =>
               API.Address.Port := API.Address.Port + 1;
               if API.Address.Port = Port_Hint then
                  raise;
                  --  Argh! we tried every possible value and
                  --  wrapped. Bail out.
               end if;
         end;
      end loop;

      --  Create Profile Factory

      if API.PF /= null then
         Create_Factory
           (API.PF.all,
            API.SAP,
            PolyORB.Components.Component_Access (Setup.The_ORB));
      end if;
   end Initialize_Unicast_Socket;

end PolyORB.Utils.UDP_Access_Points;
