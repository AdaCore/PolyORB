------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . T L S _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Components;
with PolyORB.Setup;
--  with PolyORB.TLS;
with PolyORB.Transport.Connected.Sockets.TLS;

package body PolyORB.Utils.TLS_Access_Points is

   use PolyORB.Binding_Data;
   use PolyORB.Sockets;
--   use PolyORB.TLS;
   use PolyORB.Transport;
   use PolyORB.Transport.Connected.Sockets.TLS;

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (DAP       : out Access_Point_Info;
      Address   : Inet_Addr_Type := Any_Inet_Addr;
      Port_Hint : Port_Type      := Any_Port)
   is
      Port : Port_Type := Port_Hint;
   begin
      Create_Socket (DAP.Socket);

      DAP.Address :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port,
                        Family => Family_Inet);

      --  Allow reuse of local addresses

      Set_Socket_Option
        (DAP.Socket,
         Socket_Level,
         (Reuse_Address, True));

      if DAP.SAP = null then
         DAP.SAP := new TLS_Access_Point;
      end if;

      loop
         DAP.Address.Port := Port;

         begin
            Create
              (TLS_Access_Point (DAP.SAP.all),
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
           (DAP.PF.all, DAP.SAP, Components.Component_Access (Setup.The_ORB));
      end if;
   end Initialize_Socket;

end PolyORB.Utils.TLS_Access_Points;
