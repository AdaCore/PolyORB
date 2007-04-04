------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . T C P _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2007, Free Software Foundation, Inc.          --
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

--  Utility routines to set up TCP listening sockets

with Ada.Exceptions;

with PolyORB.Components;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Transport.Connected.Sockets;

package body PolyORB.Utils.TCP_Access_Points is

   use PolyORB.Log;
   use PolyORB.Transport.Connected.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.utils.tcp_access_points");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   --  function C (Level : Log_Level := Debug) return Boolean
   --    renames L.Enabled;
   --  pragma Unreferenced (C); --  For conditional pragma Debug

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (API       : in out Access_Point_Info;
      Address   : Sockets.Inet_Addr_Type := Any_Inet_Addr;
      Port_Hint : Port_Interval)
   is
   begin
      Create_Socket (API.Socket);

      API.Address :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port_Hint.Lo,
                        Family => Family_Inet);

      --  Allow reuse of local addresses

      Set_Socket_Option
        (API.Socket,
         Socket_Level,
         (Reuse_Address, True));

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
   end Initialize_Socket;

end PolyORB.Utils.TCP_Access_Points;
