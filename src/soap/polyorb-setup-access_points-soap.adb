------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . S O A P      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  Setup for SOAP access point.

with PolyORB.Binding_Data.SOAP;
with PolyORB.Filters.HTTP;
with PolyORB.Protocols.SOAP_Pr;

with PolyORB.Parameters;
with PolyORB.Initialization;

with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Sockets;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.Utils.TCP_Access_Points;

package body PolyORB.Setup.Access_Points.SOAP is

   use PolyORB.Filters;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Utils.Socket_Access_Points;
   use PolyORB.Utils.TCP_Access_Points;

   --  The SOAP access point

   SOAP_Access_Point : Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.SOAP.SOAP_Profile_Factory);
   HTTP_Filter   : aliased PolyORB.Filters.HTTP.HTTP_Filter_Factory;
   SOAP_Protocol : aliased Protocols.SOAP_Pr.SOAP_Protocol;
   --  XXX
   --  It is not a very satisfying thing to have to declare
   --  HTTP_Filter and SOAP_Protocol explicitly on the server
   --  side. On the client side, this is done in Binding_Data.SOAP
   --  (as an effect of binding a SOAP object reference).
   --  Since Binding_Data encapsulates the association of a protocol
   --  with a complete transport stack, it should also provide
   --  the corresponding server-side primitive (eg as a constant
   --  filter chain created at initialisation.)

   SOAP_Factories : aliased Filters.Factory_Array
     := (0 => HTTP_Filter'Access, 1 => SOAP_Protocol'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points is
      use PolyORB.Parameters;

   begin
      if Get_Conf ("access_points", "soap", True) then

         declare
            Port_Hint : constant Port_Interval := To_Port_Interval
                          (Get_Conf
                           ("soap",
                            "polyorb.protocols.soap.default_port",
                            (Integer (Any_Port), Integer (Any_Port))));

            Addr : constant Inet_Addr_Type
              := Inet_Addr (String'(Get_Conf
                                    ("soap",
                                     "polyorb.protocols.soap.default_addr",
                                     Image (No_Inet_Addr))));

         begin
            Initialize_Socket (SOAP_Access_Point, Addr, Port_Hint);

            Register_Access_Point
              (ORB    => The_ORB,
               TAP    => SOAP_Access_Point.SAP,
               Chain  => SOAP_Factories'Access,
               PF     => SOAP_Access_Point.PF);
            --  Register socket with ORB object, associating a protocol
            --  to the transport service access point.
         end;
      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"access_points.soap",
       Conflicts => Empty,
       Depends   => +"orb",
       Provides  => +"access_points",
       Implicit  => False,
       Init      => Initialize_Access_Points'Access,
       Shutdown  => null));
end PolyORB.Setup.Access_Points.SOAP;
