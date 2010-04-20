------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . S S L I O P    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2010, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data.GIOP.IIOP;
with PolyORB.Filters.Slicers;
with PolyORB.GIOP_P.Transport_Mechanisms.SSLIOP;
with PolyORB.Initialization;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.Setup.Access_Points.IIOP;
with PolyORB.Sockets;
with PolyORB.SSL;
with PolyORB.Transport.Connected.Sockets.SSL;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.Utils.SSL_Access_Points;
with PolyORB.Utils.Strings;

package body PolyORB.Setup.Access_Points.SSLIOP is

   use PolyORB.Binding_Data.GIOP.IIOP;
   use PolyORB.Filters.Slicers;
   use PolyORB.GIOP_P.Transport_Mechanisms;
   use PolyORB.GIOP_P.Transport_Mechanisms.SSLIOP;
   use PolyORB.ORB;
   use PolyORB.Setup.Access_Points.IIOP;
   use PolyORB.Sockets;
   use PolyORB.SSL;
   use PolyORB.Transport.Connected.Sockets.SSL;
   use PolyORB.Utils.Socket_Access_Points;
   use PolyORB.Utils.SSL_Access_Points;

   --  The SSLIOP access point

   SSLIOP_Access_Point : Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new SSL_Access_Point,
         PF      => null);

   Sli : aliased Slicer_Factory;
   Pro : aliased PolyORB.Protocols.GIOP.IIOP.IIOP_Protocol;
   SSLIOP_Factories : aliased Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points is
      use PolyORB.Parameters;

   begin
      if Get_Conf ("access_points", "iiop", Default => True)
        and then Get_Conf ("access_points", "iiop.ssliop", Default => False)
      then
         declare
            Factory : constant Transport_Mechanism_Factory_Access :=
                                 new SSLIOP_Transport_Mechanism_Factory;

            Port_Hint : constant Port_Interval := To_Port_Interval
                          (Get_Conf
                           ("ssliop",
                            "polyorb.protocols.ssliop.default_port",
                            (Integer (Any_Port), Integer (Any_Port))));

            Addr : constant Inet_Addr_Type
              := Inet_Addr (String'(Get_Conf
                                    ("iiop",
                                     "polyorb.protocols.iiop.default_addr",
                                     Image (No_Inet_Addr))));
            --  SSLIOP share its default address with IIOP

            CA_File : constant String
               := Get_Conf
               ("ssliop",
                "polyorb.protocols.ssliop.cafile",
                "");

            Cont : SSL_Context_Type;

            Profile_Factory : PolyORB.Binding_Data.Profile_Factory_Access;

         begin
            Create_Context
              (Cont,
               Any,
               Get_Conf
                 ("ssliop",
                  "polyorb.protocols.ssliop.privatekeyfile",
                  "privkey.pem"),
               Get_Conf
                 ("ssliop",
                  "polyorb.protocols.ssliop.certificatefile",
                  "cacert.pem"),
               CA_File,
               Get_Conf
                 ("ssliop",
                  "polyorb.protocols.ssliop.capath",
                ""),
               (Get_Conf
                  ("ssliop", "polyorb.protocols.ssliop.verify", False),
                Get_Conf
                  ("ssliop",
                   "polyorb.protocols.ssliop.verify_fail_if_no_peer_cert",
                   False),
                Get_Conf
                  ("ssliop",
                   "polyorb.protocols.ssliop.verify_client_once",
                   False)));

            if CA_File /= "" then
               Load_Client_CA (Cont, CA_File);
            end if;

            Initialize_Socket (SSLIOP_Access_Point, Addr, Port_Hint, Cont);

            --  Create TM factory

            Create_Factory
              (SSLIOP_Transport_Mechanism_Factory (Factory.all),
               SSLIOP_Access_Point.SAP);

            --  Retrieve primary IIOP profile factory

            Profile_Factory := Get_Profile_Factory;

            --  Add newly created TM factory to profile factory

            Add_Transport_Mechanism_Factory
              (IIOP_Profile_Factory (Profile_Factory.all), Factory);

            if Get_Conf
                 ("ssliop",
                  "polyorb.protocols.ssliop.disable_unprotected_invocations",
                  False)
            then
               Disable_Unprotected_Invocations
                 (IIOP_Profile_Factory (Profile_Factory.all));
            else
               Profile_Factory := null;
            end if;

            Register_Access_Point
              (ORB    => The_ORB,
               TAP    => SSLIOP_Access_Point.SAP,
               Chain  => SSLIOP_Factories'Access,
               PF     => Profile_Factory);

         end;
      end if;
   end Initialize_Access_Points;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      => +"access_points.iiop.ssliop",
          Conflicts => Empty,
          Depends   => +"ssl"
                      & "orb"
                      & "protocols.giop.iiop"
                      & "access_points.iiop",
          Provides  => +"access_points",
          Implicit  => False,
          Init      => Initialize_Access_Points'Access,
          Shutdown  => null));
   end;
end PolyORB.Setup.Access_Points.SSLIOP;
