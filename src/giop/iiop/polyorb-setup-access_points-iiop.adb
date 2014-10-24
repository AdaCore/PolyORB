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
                 "");

            --  default_port is <port-interval>
            --  If present, it pro any <port-interval> from default_addr

            Port_Hint : constant Port_Interval := To_Port_Interval
                          (Get_Conf
                           ("iiop",
                            "polyorb.protocols.iiop.default_port",
                            (Integer (Any_Port), Integer (Any_Port))));

            Created_APs : constant APs :=
              Initialize_Access_Points (Primary_Addr_Str, Port_Hint);

            Primary_Mech_Factory : Transport_Mechanism_Factory_Access;
            --  Mechanism factory of the primary access point

            Alternate_Listen_Addresses : constant String :=
              Get_Conf ("iiop",
                        "polyorb.protocols.iiop.alternate_listen_addresses",
                        "");

         begin
            Primary_IIOP_AP := Created_APs (Created_APs'First);

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

            Primary_Mech_Factory := Get_Primary_Transport_Mechanism_Factory
                    (IIOP_Profile_Factory (Primary_IIOP_Profile_Factory.all));

            for  J in Created_APs'First + 1 .. Created_APs'Last loop
               --  Add further AP names to the mechanism factory of the primary
               --  profile factory.

               Create_Factory
                 (IIOP_Transport_Mechanism_Factory (Primary_Mech_Factory.all),
                  Created_APs (J));

               Register_Access_Point
                 (ORB   => The_ORB,
                  TAP   => Created_APs (J),
                  Chain => IIOP_Factories'Access,
                  PF    => null);
            end loop;

            if Alternate_Listen_Addresses /= "" then
               declare
                  First   : Positive := Alternate_Listen_Addresses'First;
                  Last    : Natural  := 0;

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

                     declare
                        Alternate_Created_APs : constant APs :=
                          Initialize_Access_Points
                            (Alternate_Listen_Addresses (First .. Last),
                             Port_Hint);

                     begin
                        for J in Alternate_Created_APs'Range loop

                           --  Add alternate AP name to the mechanism factory
                           --  of the primary profile factory.

                           Create_Factory
                             (IIOP_Transport_Mechanism_Factory
                                (Primary_Mech_Factory.all),
                              Alternate_Created_APs (J));

                           Register_Access_Point
                             (ORB   => The_ORB,
                              TAP   => Alternate_Created_APs (J),
                              Chain => IIOP_Factories'Access,
                              PF    => null);
                        end loop;
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
