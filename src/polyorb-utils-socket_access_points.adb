------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . U T I L S . S O C K E T _ A C C E S S _ P O I N T S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2013, Free Software Foundation, Inc.          --
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

--  Common definitions for all socket-based access points

with GNAT.Regpat;

with PolyORB.Transport.Sockets;

package body PolyORB.Utils.Socket_Access_Points is

   use GNAT.Regpat;

   use PolyORB.Transport.Sockets;

   Listen_Matcher : constant Pattern_Matcher :=
                      Compile ("^([^:\[]*)(\[[^\]]*\])?(:[0-9-]*)?");
   subtype Listen_Match is Match_Array (0 .. Paren_Count (Listen_Matcher));

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   function Initialize_Access_Points
     (Listen_Spec   : String;
      Default_Ports : Port_Interval := (Any_Port, Any_Port);
      Initialize_AP : access function
        (Addr      : Inet_Addr_Type;
         Port_Hint : Port_Interval) return Transport_Access_Point_Access)
      return APs
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

         function Setup_AP
           (Addr      : Inet_Addr_Type;
            Port_Hint : Port_Interval) return Transport_Access_Point_Access;
         --  Initialize SAP and set its published address

         --------------
         -- Setup_AP --
         --------------

         function Setup_AP
           (Addr      : Inet_Addr_Type;
            Port_Hint : Port_Interval) return Transport_Access_Point_Access
         is
            AP   : constant Transport_Access_Point_Access :=
                     Initialize_AP (Addr, Port_Hint);
            SAP  : Socket_Access_Point'Class
                     renames Socket_Access_Point'Class (AP.all);
            S_Addr : constant Sock_Addr_Type := SAP.Socket_AP_Address;
         begin
            SAP.Set_Socket_AP_Publish_Name
              (Publish_Name (Pub_Spec, S_Addr.Addr) + S_Addr.Port);
            return AP;
         end Setup_AP;

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

         --  If Bind_Spec is an IP address, use it

         if Is_IP_Address (Bind_Spec) then
            return APIs : APs (1 .. 1) do
               APIs (1) := Setup_AP (Inet_Addr (Bind_Spec), Port_Hint);
            end return;

         --  If Bind_Spec is a name, resolve and bind to all returned
         --  addresses.

         elsif Bind_Spec /= "" then
            declare
               H : constant Host_Entry_Type := Get_Host_By_Name (Bind_Spec);
            begin
               return APIs : APs (1 .. H.Addresses_Length) do
                  for J in 1 .. H.Addresses_Length loop
                     APIs (J) := Setup_AP (Addresses (H, J), Port_Hint);
                  end loop;
               end return;
            end;

         --  Here if no Bind_Spec: bind to Any_Inet_Addr

         else
            return APIs : APs (1 .. 1) do
               APIs (1) := Setup_AP (Any_Inet_Addr, Port_Hint);
            end return;
         end if;
      end;
   end Initialize_Access_Points;

   ------------------
   -- Publish_Name --
   ------------------

   function Publish_Name
     (Publish : String;
      Addr    : Inet_Addr_Type) return String
   is
   begin
      if Publish /= "" and then Publish /= Image (Any_Inet_Addr) then
         return Publish;

      --  If bound to a specific IP address, return it

      elsif Addr /= Any_Inet_Addr then
         return Image (Addr);

      --  Else return our best guess for our own address

      else
         return Image (Local_Inet_Address);
      end if;
   end Publish_Name;

   ------------------------------
   -- Set_Default_Publish_Name --
   ------------------------------

   procedure Set_Default_Publish_Name
     (Publish : in out Socket_Name_Ptr;
      Addr    : Sock_Addr_Type)
   is
   begin
      if Publish = null then
         Publish := new Socket_Name'(Publish_Name ("", Addr.Addr) + Addr.Port);
      end if;
   end Set_Default_Publish_Name;

   ----------------------
   -- To_Port_Interval --
   ----------------------

   function To_Port_Interval (I : Interval) return Port_Interval is
   begin
      return (Lo => Port_Type (I.Lo), Hi => Port_Type (I.Hi));
   end To_Port_Interval;

end PolyORB.Utils.Socket_Access_Points;
