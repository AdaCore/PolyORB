------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . S E T U P . T C P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
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

with PolyORB.Components;
with PolyORB.Transport.Sockets;

package body PolyORB.Setup.TCP_Access_Points is

   use PolyORB.Transport.Sockets;

   procedure Initialize_Socket
     (DAP  : in out Access_Point_Info;
      Port_Hint : in Port_Type)
   is
      Port : Port_Type := Port_Hint;
   begin
      Create_Socket (DAP.Socket);

      DAP.Address.Addr := Any_Inet_Addr;

      --  Allow reuse of local addresses.

      Set_Socket_Option
        (DAP.Socket,
         Socket_Level,
         (Reuse_Address, True));

      if DAP.SAP = null then
         DAP.SAP := new Transport.Sockets.Socket_Access_Point;
      end if;
      loop
         DAP.Address.Port := Port;
         begin
            Create
              (Socket_Access_Point (DAP.SAP.all),
               DAP.Socket,
               DAP.Address);
            exit;
         exception
            when Sockets.Socket_Error =>
               Port := Port + 1;
               if Port = Port_Hint then
                  raise;
                  --  Argh! we tried every possible value and
                  --  wrapped. Bail out.
               end if;
         end;
      end loop;
      if DAP.PF /= null then
         Create_Factory
           (DAP.PF.all, DAP.SAP, Components.Component_Access (The_ORB));
      end if;
   end Initialize_Socket;

end PolyORB.Setup.TCP_Access_Points;
