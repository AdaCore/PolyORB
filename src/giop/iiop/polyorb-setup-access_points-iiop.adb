------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . I I O P      --
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

--  Setup for IIOP access point

with PolyORB.Binding_Data.GIOP.IIOP;
with PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
with PolyORB.Protocols.GIOP;
with PolyORB.Protocols.GIOP.IIOP;

with PolyORB.Parameters;
with PolyORB.Filters;
with PolyORB.Filters.Slicers;
with PolyORB.Initialization;

with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Sockets;
with PolyORB.Transport;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.Utils.TCP_Access_Points;

package body PolyORB.Setup.Access_Points.IIOP is

   use PolyORB.Binding_Data.GIOP.IIOP;
   use PolyORB.Filters;
   use PolyORB.Filters.Slicers;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.GIOP_P.Transport_Mechanisms.IIOP;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Utils;
   use PolyORB.Utils.Socket_Access_Points;
   use PolyORB.Utils.TCP_Access_Points;

   --  The IIOP access point

   Primary_IIOP_AP : Transport.Transport_Access_Point_Access;
   Primary_IIOP_Profile_Factory : Binding_Data.Profile_Factory_Access;

   Sli : aliased Slicer_Factory;
   Pro : aliased Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : aliased Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   -------------------------
   -- Get_Profile_Factory --
   -------------------------

   function Get_Profile_Factory return Binding_Data.Profile_Factory_Access is
   begin
      return Primary_IIOP_Profile_Factory;
   end Get_Profile_Factory;

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points is
      use PolyORB.Parameters;

   begin
      if Get_Conf ("access_points", "iiop", True) then
         declare
            --  default_addr is <host>[:<port-interval>]

            Primary_Addr_Str : constant String :=
              Get_Conf
                ("iiop",
                 "polyorb.protocols.iiop.default_addr",
                 Image (No_Inet_Addr));
            Primary_Addr_Sep : constant Natural :=
              Find (Primary_Addr_Str,
                    Start => Primary_Addr_Str'First,
                    What  => ':');

            --  default_port is <port-interval>
            --  If present, it pro any <port-interval> from default_addr

            Port_Hint : Port_Interval := To_Port_Interval
                          (Get_Conf
                           ("iiop",
                            "polyorb.protocols.iiop.default_port",
                            (Integer (Any_Port), Integer (Any_Port))));

            Primary_Addr : constant Inet_Addr_Type :=
              Inet_Addr (Primary_Addr_Str
                           (Primary_Addr_Str'First ..
                            Primary_Addr_Sep - 1));

            Alternate_Listen_Addresses : constant String
              := Get_Conf ("iiop",
                           "polyorb.protocols.iiop.alternate_listen_addresses",
                           "");

         begin
            if Port_Hint = (Any_Port, Any_Port)
                 and then Primary_Addr_Sep < Primary_Addr_Str'Last
            then
               Port_Hint :=
                 To_Port_Interval
                   (To_Interval (Primary_Addr_Str (Primary_Addr_Sep + 1
                                                .. Primary_Addr_Str'Last)));
            end if;

            Initialize_Socket (Primary_IIOP_AP, Primary_Addr, Port_Hint);
            Primary_IIOP_Profile_Factory :=
              new IIOP_Profile_Factory'(Create_Factory (Primary_IIOP_AP));

            if Get_Conf
               ("ssliop",
                "polyorb.protocols.ssliop.disable_unprotected_invocations",
                False)
            then
               return;
            end if;

            Register_Access_Point
              (ORB   => The_ORB,
               TAP   => Primary_IIOP_AP,
               Chain => IIOP_Factories'Access,
               PF    => Primary_IIOP_Profile_Factory);

            if Alternate_Listen_Addresses /= "" then
               declare
                  Factory : constant Transport_Mechanism_Factory_Access
                    := Get_Primary_Transport_Mechanism_Factory
                    (IIOP_Profile_Factory
                     (Primary_IIOP_Profile_Factory.all));
                  First   : Positive := Alternate_Listen_Addresses'First;
                  Last    : Natural  := 0;
                  Delim   : Natural  := 0;

               begin
                  while First <= Alternate_Listen_Addresses'Last loop
                     --  Skip all spaces

                     for J in First .. Alternate_Listen_Addresses'Last loop
                        if Alternate_Listen_Addresses (J) /= ' ' then
                           First := J;
                           exit;
                        end if;
                     end loop;

                     --  Find end of address

                     for J in First .. Alternate_Listen_Addresses'Last loop
                        if Alternate_Listen_Addresses (J) = ' ' then
                           Last := J - 1;
                           exit;

                        elsif J = Alternate_Listen_Addresses'Last then
                           Last := J;
                        end if;
                     end loop;

                     --  Find host/port delimiter

                     Delim := Last + 1;

                     for J in First .. Last loop
                        if Alternate_Listen_Addresses (J) = ':' then
                           Delim := J;
                           exit;
                        end if;
                     end loop;

                     --  Create transport mechanism factory, create transport
                     --  access point and register it.

                     declare
                        Alternate_IIOP_Access_Point :
                          Transport.Transport_Access_Point_Access;

                        Alternate_Addr : constant Inet_Addr_Type :=
                          Inet_Addr
                            (Alternate_Listen_Addresses (First .. Delim - 1));

                        Alternate_Port : Port_Interval := (Any_Port, Any_Port);

                     begin
                        if Delim < Last then
                           Alternate_Port.Lo :=
                             Port_Type'Value
                               (Alternate_Listen_Addresses
                                  (Delim + 1 .. Last));
                           Alternate_Port.Hi := Alternate_Port.Lo;
                        end if;

                        if Alternate_Addr /= No_Inet_Addr then
                           Initialize_Socket
                             (Alternate_IIOP_Access_Point,
                              Alternate_Addr,
                              Alternate_Port);

                           --  Add alternate AP name to the mechanism factory
                           --  of the primary profile factory.

                           Create_Factory
                             (IIOP_Transport_Mechanism_Factory (Factory.all),
                              Alternate_IIOP_Access_Point);

                           Register_Access_Point
                             (ORB   => The_ORB,
                              TAP   => Alternate_IIOP_Access_Point,
                              Chain => IIOP_Factories'Access,
                              PF    => null);
                        end if;
                     end;

                     First := Last + 1;
                  end loop;
               end;
            end if;
         end;
      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"access_points.iiop",
       Conflicts => Empty,
       Depends   => +"sockets"
                   & "orb"
                   & "protocols.giop.iiop",
       Provides  => +"access_points",
       Implicit  => False,
       Init      => Initialize_Access_Points'Access,
       Shutdown  => null));
end PolyORB.Setup.Access_Points.IIOP;
