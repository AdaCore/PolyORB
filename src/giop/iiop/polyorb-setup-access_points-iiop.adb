------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . I I O P      --
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

--  Setup for IIOP access point.

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
with PolyORB.Transport.Connected.Sockets;
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
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Utils.Socket_Access_Points;
   use PolyORB.Utils.TCP_Access_Points;

   --  The IIOP access point

   Primary_IIOP_Access_Point : Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new PolyORB.Binding_Data.GIOP.IIOP.IIOP_Profile_Factory);

   Sli : aliased Slicer_Factory;
   Pro : aliased Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : aliased Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   -------------------------
   -- Get_Profile_Factory --
   -------------------------

   function Get_Profile_Factory
     return PolyORB.Binding_Data.Profile_Factory_Access is
   begin
      return Primary_IIOP_Access_Point.PF;
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
            Port_Hint : constant Port_Interval := To_Port_Interval
                          (Get_Conf
                           ("iiop",
                            "polyorb.protocols.iiop.default_port",
                            (Integer (Any_Port), Integer (Any_Port))));

            Primary_Addr : constant Inet_Addr_Type
              := Inet_Addr (String'(Get_Conf
                                    ("iiop",
                                     "polyorb.protocols.iiop.default_addr",
                                     Image (No_Inet_Addr))));

            Alternate_Listen_Addresses : constant String
              := Get_Conf ("iiop",
                           "polyorb.protocols.iiop.alternate_listen_addresses",
                           "");

         begin
            Initialize_Socket
              (Primary_IIOP_Access_Point, Primary_Addr, Port_Hint);

            if Get_Conf
               ("ssliop",
                "polyorb.protocols.ssliop.disable_unprotected_invocations",
                False)
            then
               return;
            end if;

            Register_Access_Point
              (ORB    => The_ORB,
               TAP    => Primary_IIOP_Access_Point.SAP,
               Chain  => IIOP_Factories'Access,
               PF     => Primary_IIOP_Access_Point.PF);

            if Alternate_Listen_Addresses /= "" then
               declare
                  Factory : constant Transport_Mechanism_Factory_Access
                    := Get_Primary_Transport_Mechanism_Factory
                    (IIOP_Profile_Factory
                     (Primary_IIOP_Access_Point.PF.all));
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
                        Alternate_IIOP_Access_Point : Access_Point_Info :=
                             (Socket  => No_Socket,
                              Address => No_Sock_Addr,
                              SAP     => new Socket_Access_Point,
                              PF      => null);

                        Alternate_Addr : constant Inet_Addr_Type :=
                                           Inet_Addr
                                             (Alternate_Listen_Addresses
                                               (First .. Delim - 1));

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

                           Create_Factory
                             (IIOP_Transport_Mechanism_Factory (Factory.all),
                              Alternate_IIOP_Access_Point.SAP);

                           Register_Access_Point
                             (ORB    => The_ORB,
                              TAP    => Alternate_IIOP_Access_Point.SAP,
                              Chain  => IIOP_Factories'Access,
                              PF     => null);
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
