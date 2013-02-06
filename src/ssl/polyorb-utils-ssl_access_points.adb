------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . U T I L S . S S L _ A C C E S S _ P O I N T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
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

with PolyORB.Transport.Connected.Sockets.SSL;

package body PolyORB.Utils.SSL_Access_Points is

   use PolyORB.Sockets;
   use PolyORB.SSL;
   use PolyORB.Transport;
   use PolyORB.Transport.Connected.Sockets.SSL;

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (SAP       : Transport.Transport_Access_Point_Access;
      Address   : Inet_Addr_Type;
      Port_Hint : Port_Interval;
      Context   : SSL_Context_Type)
   is
      Socket : Socket_Type;
      Addr   : Sock_Addr_Type;
   begin

      --  ??? Most of the code below is copied directly from TCP_Access_Points
      --  and should be factored.

      Create_Socket (Socket);

      Addr :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port_Hint.Lo,
                        Family => Family_Inet);

      --  Allow reuse of local addresses

      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));

      loop
         begin
            Create
              (SSL_Access_Point (SAP.all),
               Socket,
               Addr,
               Context);
            exit;
         exception
            when Sockets.Socket_Error =>

               --  If a specific port range was given, try next port in range

               if Addr.Port /= Any_Port
                 and then Addr.Port < Port_Hint.Hi
               then
                  Addr.Port := Addr.Port + 1;
               else
                  raise;
               end if;
         end;
      end loop;
   end Initialize_Socket;

end PolyORB.Utils.SSL_Access_Points;
