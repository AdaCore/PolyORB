------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E T U P . U D P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                             . S O C K E T S                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2002 Free Software Foundation, Inc.           --
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

--  Helper subprograms to set up access points based on normal UDP sockets
--  for a PolyORB server.
--  Do NOT use for multicast sockets.

with PolyORB.Components;
with PolyORB.Sockets;
with PolyORB.Transport.Datagram.Sockets_In;

package body PolyORB.Setup.UDP_Access_Points.Sockets is

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (API       : in out UDP_Access_Point_Info;
      Port_Hint : in     Port_Type)
   is
      use PolyORB.Transport.Datagram.Sockets_In;

   begin
      --  Create Socket
      Initialize_Socket (API);

      --  Find a free port, search begin at Port_Hint
      API.Address.Addr := Any_Inet_Addr;
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
            PolyORB.Components.Component_Access (The_ORB));
      end if;
   end Initialize_Socket;

end PolyORB.Setup.UDP_Access_Points.Sockets;
