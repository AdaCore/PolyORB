------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . T L S I O P    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with PolyORB.Filters.Slicers;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.Security.Security_Manager;
with PolyORB.Security.Transport_Mechanisms.TLS;
with PolyORB.Sockets;
with PolyORB.Transport.Connected.Sockets.TLS;
with PolyORB.Utils.TLS_Access_Points;
with PolyORB.Utils.Strings;

package body PolyORB.Setup.Access_Points.TLSIOP is

   procedure Initialize;

   function Create_Target_Transport_Mechanism
     (Section_Name : String)
     return
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access;

   Sli : aliased PolyORB.Filters.Slicers.Slicer_Factory;
   Pro : aliased PolyORB.Protocols.GIOP.IIOP.IIOP_Protocol;
   IIOP_Factories : aliased Filters.Factory_Array
     := (0 => Sli'Access, 1 => Pro'Access);

   ---------------------------------------
   -- Create_Target_Transport_Mechanism --
   ---------------------------------------

   function Create_Target_Transport_Mechanism
     (Section_Name : String)
     return
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access
   is

      use PolyORB.Parameters;
      use PolyORB.Security.Transport_Mechanisms;
      use PolyORB.Security.Transport_Mechanisms.TAP_Lists;
      use PolyORB.Security.Transport_Mechanisms.TLS;
      use PolyORB.Sockets;
      use PolyORB.Transport.Connected.Sockets.TLS;
      use PolyORB.Utils.TLS_Access_Points;

      Addresses : constant String
        := Get_Conf (Section_Name, "addresses", "");
      Addr      : Inet_Addr_Type := No_Inet_Addr;
      Port      : Port_Type      := Any_Port;
      Result    : constant Target_Transport_Mechanism_Access
        := new Target_TLS_Transport_Mechanism;
      Point     : Access_Point_Info
        := (Socket  => No_Socket,
            Address => No_Sock_Addr,
            SAP     => null,
            PF      => null);

   begin
      if Addresses = "" then
         Initialize_Socket (Point, Addr, Port);

         PolyORB.ORB.Register_Access_Point
           (ORB   => PolyORB.Setup.The_ORB,
            TAP   => Point.SAP,
            Chain => IIOP_Factories'Access,
            PF    => null);

         Set_Transport_Mechanism (TLS_Access_Point (Point.SAP.all), Result);

         Append (Target_TLS_Transport_Mechanism (Result.all).TAP, Point.SAP);

      else
         declare
            First : Positive := Addresses'First;
            Last  : Natural  := 0;
            Delim : Natural  := 0;

         begin
            while First <= Addresses'Last loop
               --  Skip all spaces

               while First <= Addresses'Last
                 and then Addresses (First) = ' '
               loop
                  First := First + 1;
               end loop;

               --  Find end of address

               for J in First .. Addresses'Last loop
                  if Addresses (J) = ' ' then
                     Last := J - 1;
                     exit;

                  elsif J = Addresses'Last then
                     Last := J;
                  end if;
               end loop;

               --  Find host/port delimiter

               Delim := Last + 1;

               for J in First .. Last loop
                  if Addresses (J) = ':' then
                     Delim := J;
                     exit;
                  end if;
               end loop;

               --  Create transport access point and register it

               Addr  := Inet_Addr (Addresses (First .. Delim - 1));

               if Delim < Last then
                  Port := Port_Type'Value (Addresses (Delim + 1 .. Last));

               else
                  Port      := Any_Port;
               end if;

               Point := (No_Socket, No_Sock_Addr, null, null);

               if Addr /= No_Inet_Addr then
                  Initialize_Socket (Point, Addr, Port);

                  PolyORB.ORB.Register_Access_Point
                    (ORB   => PolyORB.Setup.The_ORB,
                     TAP   => Point.SAP,
                     Chain => IIOP_Factories'Access,
                     PF    => null);

                  Set_Transport_Mechanism
                    (TLS_Access_Point (Point.SAP.all), Result);

                  Append
                    (Target_TLS_Transport_Mechanism (Result.all).TAP,
                     Point.SAP);
               end if;

               First := Last + 1;
            end loop;
         end;
      end if;

      return Result;
   end Create_Target_Transport_Mechanism;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      PolyORB.Security.Security_Manager.Register_Transport_Mechanism
       ("tlsiop", Create_Target_Transport_Mechanism ("tlsiop"));
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;

   begin
      Register_Module
        (Module_Info'
         (Name      =>
          +"polyorb.setup.access_points.tlsiop",
          Conflicts => PolyORB.Initialization.String_Lists.Empty,
          Depends   => +"orb"
          & "polyorb.security.security_manager"
          & "polyorb.setup.tlsiop",
          Provides  => PolyORB.Initialization.String_Lists.Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.Setup.Access_Points.TLSIOP;
