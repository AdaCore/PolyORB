------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . S E T U P . T C P _ A C C E S S _ P O I N T S . S R P   --
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

--  Setup for SRP access point.

--  $Id$

with PolyORB.Binding_Data.SRP;
with PolyORB.Protocols.SRP;

with PolyORB.Configuration;
with PolyORB.Filters;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Transport.Sockets;
with PolyORB.Utils.Strings;

package body PolyORB.Setup.TCP_Access_Points.SRP is

   use PolyORB.Filters;
   use PolyORB.ORB;
   use PolyORB.Transport.Sockets;

   --  The 'SRP' access point.

   SRP_Access_Point : Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.SRP.SRP_Profile_Factory);

   SRP_Protocol  : aliased Protocols.SRP.SRP_Protocol;

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Configuration;
   begin
      if Get_Conf ("access_points", "srp", True) then

         Initialize_Socket (SRP_Access_Point, Any_Port);
         Register_Access_Point
           (ORB    => The_ORB,
            TAP    => SRP_Access_Point.SAP,
            Chain  => SRP_Protocol'Unchecked_Access,
            PF     => SRP_Access_Point.PF);
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
      (Name => +"tcp_access_points.srp",
       Conflicts => String_Lists.Empty,
       Depends => +"orb",
       Provides => String_Lists.Empty,
       Init => Initialize_Access_Points'Access));


end PolyORB.Setup.TCP_Access_Points.SRP;
