------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SETUP.TCP_ACCESS_POINTS.SOAP                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Setup for SOAP access point.

--  $Id$

with PolyORB.Binding_Data.SOAP;
with PolyORB.Filters.HTTP;
with PolyORB.Protocols.SOAP_Pr;

with PolyORB.Configuration;
with PolyORB.Filters;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Transport.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Setup.TCP_Access_Points.SOAP is

   use PolyORB.Filters;
   use PolyORB.ORB;
   use PolyORB.Transport.Sockets;

   --  The 'SOAP' access point.

   SOAP_Access_Point : Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.SOAP.SOAP_Profile_Factory);
   HTTP_Filter   : aliased PolyORB.Filters.HTTP.HTTP_Filter_Factory;
   SOAP_Protocol : aliased Protocols.SOAP_Pr.SOAP_Protocol;
   --  XXX
   --  It is not a very satisfying thing to have to chain
   --  HTTP_Filter and SOAP_Protocol explicitly on the server
   --  side. On the client side, this is done in Binding_Data.SOAP
   --  (as an effect of binding a SOAP object reference).
   --  Since Binding_Data encapsulates the association of a protocol
   --  with a complete transport stack, it should also provide
   --  the corresponding server-side primitive (eg as a constant
   --  filter chain created at initialisation.)

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Configuration;
   begin
      if Get_Conf ("access_points", "soap", True) then

         Initialize_Socket (SOAP_Access_Point, 8080);
         Chain_Factories
           ((0 => HTTP_Filter'Unchecked_Access,
             1 => SOAP_Protocol'Unchecked_Access));
         Register_Access_Point
           (ORB    => The_ORB,
            TAP    => SOAP_Access_Point.SAP,
            Chain  => HTTP_Filter'Unchecked_Access,
            PF     => SOAP_Access_Point.PF);
         --  Register socket with ORB object, associating a protocol
         --  to the transport service access point.
      end if;
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"tcp_access_points.soap",
       Conflicts => String_Lists.Empty,
       Depends => +"orb",
       Provides => String_Lists.Empty,
       Init => Initialize_Access_Points'Access));

end PolyORB.Setup.TCP_Access_Points.SOAP;
