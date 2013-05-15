------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . T L S I O P    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2013, Free Software Foundation, Inc.          --
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

with PolyORB.Filters.Slicers;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Parameters;
with PolyORB.Protocols.GIOP.IIOP;
with PolyORB.Security.Security_Manager;
with PolyORB.Security.Transport_Mechanisms.TLS;
with PolyORB.Sockets;
with PolyORB.Transport.Connected.Sockets.TLS;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.Utils.TLS_Access_Points;
with PolyORB.Utils.Strings;

package body PolyORB.Setup.Access_Points.TLSIOP is

   use PolyORB.Transport;
   use PolyORB.Utils.Socket_Access_Points;

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
     return Security.Transport_Mechanisms.Target_Transport_Mechanism_Access
   is
      use PolyORB.Parameters;
      use PolyORB.Security.Transport_Mechanisms;
      use PolyORB.Security.Transport_Mechanisms.TAP_Lists;
      use PolyORB.Security.Transport_Mechanisms.TLS;
      use PolyORB.Sockets;
      use PolyORB.Transport.Connected.Sockets.TLS;
      use PolyORB.Utils.TLS_Access_Points;

      Addresses : constant String :=
                    Get_Conf (Section_Name, "addresses", "");

      Result    : constant Target_Transport_Mechanism_Access :=
                    new Target_TLS_Transport_Mechanism;

      function Initialize_TLS_AP
        (Addr      : Inet_Addr_Type;
         Port_Hint : Port_Interval) return Transport_Access_Point_Access;
      --  Initialize TLS access point

      -----------------------
      -- Initialize_TLS_AP --
      -----------------------

      function Initialize_TLS_AP
        (Addr      : Inet_Addr_Type;
         Port_Hint : Port_Interval) return Transport_Access_Point_Access
      is
      begin
         return AP : constant Transport_Access_Point_Access :=
           new TLS_Access_Point
         do
            Initialize_Socket (AP, Addr, Port_Hint);
         end return;
      end Initialize_TLS_AP;

      Created_APs : constant APs :=
        Initialize_Access_Points
          (Addresses, (Any_Port, Any_Port), Initialize_TLS_AP'Access);

   begin
      for J in Created_APs'Range loop
         PolyORB.ORB.Register_Access_Point
           (ORB   => PolyORB.Setup.The_ORB,
            TAP   => Created_APs (J),
            Chain => IIOP_Factories'Access,
            PF    => null);

         Set_Transport_Mechanism
           (TLS_Access_Point (Created_APs (J).all), Result);

         Append
           (Target_TLS_Transport_Mechanism (Result.all).TAP, Created_APs (J));
      end loop;

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
