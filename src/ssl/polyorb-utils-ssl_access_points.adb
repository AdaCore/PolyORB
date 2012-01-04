------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . S S L _ A C C E S S _ P O I N T S       --
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
with PolyORB.Transport.Connected.Sockets.SSL;

package body PolyORB.Utils.SSL_Access_Points is

   use PolyORB.Binding_Data;
   use PolyORB.Sockets;
   use PolyORB.SSL;
   use PolyORB.Transport;
   use PolyORB.Transport.Connected.Sockets.SSL;

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (API       : out Access_Point_Info;
      Address   : Inet_Addr_Type;
      Port_Hint : Port_Interval;
      Context   : SSL_Context_Type)
   is
   begin
      --  ??? Most of the code below is copied directly from TCP_Access_Points
      --  and should be factored.

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
         API.SAP := new SSL_Access_Point;
      end if;

      loop
         begin
            Create
              (SSL_Access_Point (API.SAP.all),
               API.Socket,
               API.Address,
               Context);
            exit;
         exception
            when Sockets.Socket_Error =>

               --  If a specific port range was given, try next port in range

               if API.Address.Port /= Any_Port
                 and then API.Address.Port < Port_Hint.Hi
               then
                  API.Address.Port := API.Address.Port + 1;
               else
                  raise;
               end if;

         end;
      end loop;

      if API.PF /= null then
         Create_Factory
           (API.PF.all, API.SAP, Components.Component_Access (Setup.The_ORB));
      end if;
   end Initialize_Socket;

end PolyORB.Utils.SSL_Access_Points;
