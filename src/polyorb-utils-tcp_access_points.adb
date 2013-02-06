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

pragma Ada_2005;

with Ada.Exceptions;

with GNAT.Regpat;

with PolyORB.Log;
with PolyORB.Transport.Connected.Sockets;

package body PolyORB.Utils.TCP_Access_Points is

   use GNAT.Regpat;
   use PolyORB.Log;
   use PolyORB.Transport.Connected.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.utils.tcp_access_points");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   --  function C (Level : Log_Level := Debug) return Boolean
   --    renames L.Enabled;

   Listen_Matcher : constant Pattern_Matcher :=
                      Compile ("^([^:\]]*)(\[[^\]]*\])?(:[0-9-]*)?");
   subtype Listen_Match is Match_Array (0 .. Paren_Count (Listen_Matcher));

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   function Initialize_Access_Points
     (Listen_Spec   : String;
      Default_Ports : Port_Interval := (Any_Port, Any_Port)) return AP_Infos
   is
      M : Listen_Match;
   begin
      Match (Listen_Matcher, Listen_Spec, M);

      declare
         Bind_Spec : constant String :=
                       Listen_Spec (M (1).First .. M (1).Last);
         --  Specification of address to bind to:
         --  * IP address (dotted quad)
         --  * host name (bind to all associated addresses)
         --  * empty, bind to Any_Inet_Addr

         function Pub_Spec return String;
         --  Name to be published in profiles:
         --  * defaults to Bind_Spec
         --  * if empty, publish primary non-loopback IP address

         --------------
         -- Pub_Spec --
         --------------

         function Pub_Spec return String is
         begin
            if M (2).Last > M (2).First then
               return Listen_Spec (M (2).First + 1 .. M (2).Last - 1);
            else
               return Bind_Spec;
            end if;
         end Pub_Spec;

         Port_Hint : Port_Interval;

      --  Start of processing for Initialize_Access_Points

      begin
         --  If the Listen_Spec specifies a port [interval], use it, else
         --  use Default_Ports.

         if M (3).Last > M (3).First then
            Port_Hint := To_Port_Interval
              (To_Interval (Listen_Spec (M (3).First + 1 .. M (3).Last)));
         else
            Port_Hint := Default_Ports;
         end if;

         --  If a Bind_Spec is present, resolve and bind to all returned
         --  addresses.

         if Bind_Spec /= "" then
            declare
               H : constant Host_Entry_Type := Get_Host_By_Name (Bind_Spec);
            begin
               return APIs : AP_Infos (1 .. H.Addresses_Length) do
                  for J in 1 .. H.Addresses_Length loop
                     Initialize_Socket
                       (APIs (J),
                        Addresses (H, J), Port_Hint,
                        Pub_Spec);
                  end loop;
               end return;
            end;

         --  Here if no Bind_Spec: bind to Any_Inet_Addr

         else
            return APIs : AP_Infos (1 .. 1) do
               Initialize_Socket
                 (APIs (1), Any_Inet_Addr, Port_Hint, Pub_Spec);
            end return;
         end if;
      end;
   end Initialize_Access_Points;

   -----------------------
   -- Initialize_Socket --
   -----------------------

   procedure Initialize_Socket
     (SAP       : out Transport_Access_Point_Access;
      Address   : Sockets.Inet_Addr_Type := Any_Inet_Addr;
      Port_Hint : Port_Interval;
      Publish   : String := "")
   is
      Socket  : Socket_Type;
      S_Addr  : Sock_Addr_Type;
   begin
      Create_Socket (Socket);

      S_Addr :=
        Sock_Addr_Type'(Addr   => Address,
                        Port   => Port_Hint.Lo,
                        Family => Family_Inet);

      --  Allow reuse of local addresses

      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));

      SAP := new Socket_Access_Point;

      loop
         begin
            Create
              (Socket_Access_Point (SAP.all),
               Socket,
               S_Addr,
               Publish);
            exit;
         exception
            when E : Sockets.Socket_Error =>

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
   end Initialize_Socket;

end PolyORB.Utils.TCP_Access_Points;
