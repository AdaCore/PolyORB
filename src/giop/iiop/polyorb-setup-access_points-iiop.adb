------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--     P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . I I O P      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  Setup for IIOP access point.

with PolyORB.Binding_Data.IIOP;
with PolyORB.Protocols.GIOP;
with PolyORB.Protocols.GIOP.IIOP;

with PolyORB.Parameters;
with PolyORB.Filters;
with PolyORB.Filters.Slicers;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Sockets;
with PolyORB.Transport.Connected.Sockets;
with PolyORB.Utils.Strings;
with PolyORB.Utils.TCP_Access_Points;

package body PolyORB.Setup.Access_Points.IIOP is

   use PolyORB.Filters;
   use PolyORB.Filters.Slicers;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport.Connected.Sockets;
   use PolyORB.Utils.TCP_Access_Points;

   --  The 'GIOP' access point.

   GIOP_Access_Point : Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_Access_Point,
         PF      => new Binding_Data.IIOP.IIOP_Profile_Factory);

   IIOP_Pro : aliased Protocols.GIOP.IIOP.IIOP_Protocol;
   Sli      : aliased Slicer_Factory;

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Parameters;
   begin
      if Get_Conf ("access_points", "iiop", True) then

         Initialize_Socket (GIOP_Access_Point, Any_Port);

         Chain_Factories ((0 => Sli'Unchecked_Access,
                           1 => IIOP_Pro'Unchecked_Access));

         Register_Access_Point
           (ORB    => The_ORB,
            TAP    => GIOP_Access_Point.SAP,
            Chain  => Sli'Unchecked_Access,
            PF     => GIOP_Access_Point.PF);
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
       Init      => Initialize_Access_Points'Access));
end PolyORB.Setup.Access_Points.IIOP;
