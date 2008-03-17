------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . U I P M C     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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

--  Setup socket for UIPMC

with PolyORB.Binding_Data.GIOP.UIPMC;
with PolyORB.Filters;
with PolyORB.Filters.Fragmenter;
with PolyORB.Filters.MIOP.MIOP_In;
with PolyORB.Initialization;

with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols;
with PolyORB.Protocols.GIOP.UIPMC;
with PolyORB.Sockets;
with PolyORB.Transport.Datagram.Sockets_In;
with PolyORB.Utils.Strings;
with PolyORB.Utils.UDP_Access_Points;

package body PolyORB.Setup.Access_Points.UIPMC is

   use PolyORB.Filters;
   use PolyORB.Filters.Fragmenter;
   use PolyORB.Filters.MIOP.MIOP_In;
   use PolyORB.ORB;
   use PolyORB.Sockets;
   use PolyORB.Transport.Datagram.Sockets_In;
   use PolyORB.Utils.UDP_Access_Points;

   UIPMC_Access_Point : UDP_Access_Point_Info
     := (Socket  => No_Socket,
         Address => No_Sock_Addr,
         SAP     => new Socket_In_Access_Point,
         PF      => new PolyORB.Binding_Data.GIOP.UIPMC.UIPMC_Profile_Factory);

   Fra : aliased Fragmenter_Factory;
   Min : aliased MIOP_In_Factory;
   Pro : aliased Protocols.GIOP.UIPMC.UIPMC_Protocol;
   UIPMC_Factories : aliased Filters.Factory_Array
     := (0 => Fra'Access, 1 => Min'Access, 2 => Pro'Access);

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points
   is
      use PolyORB.Parameters;

      Addr : constant String :=
               Get_Conf ("miop", "polyorb.miop.multicast_addr", "");
      Port : constant Port_Type :=
               Port_Type (Get_Conf ("miop", "polyorb.miop.multicast_port", 0));
   begin
      if Get_Conf ("access_points", "uipmc", True) then

         if Addr = "" or else Port = 0 then
            return;
         end if;

         Initialize_Multicast_Socket
           (UIPMC_Access_Point, Inet_Addr (Addr), Port);

         Register_Access_Point
           (ORB    => The_ORB,
            TAP    => UIPMC_Access_Point.SAP,
            Chain  => UIPMC_Factories'Access,
            PF     => UIPMC_Access_Point.PF);
      end if;

   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"access_points.uipmc",
       Conflicts => String_Lists.Empty,
       Depends   => +"orb" & "sockets",
       Provides  => String_Lists.Empty,
       Implicit  => False,
       Init      => Initialize_Access_Points'Access,
       Shutdown  => null));
end PolyORB.Setup.Access_Points.UIPMC;
