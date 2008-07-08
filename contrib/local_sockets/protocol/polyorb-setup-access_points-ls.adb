------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . S E T U P . A C C E S S _ P O I N T S . L S        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data.GIOP.Local_Sockets;
with PolyORB.Protocols.GIOP.Local_Sockets;
with PolyORB.Transport.Connected.Local_Sockets;

with PolyORB.Components;
with PolyORB.Filters.Slicers;
with PolyORB.Initialization;
with PolyORB.ORB;
with PolyORB.Protocols;
with PolyORB.Setup;
with PolyORB.Utils.Strings;
with PolyORB.Log;
with PolyORB.Local_Sockets;
pragma Elaborate_All (PolyORB.Local_Sockets);

package body PolyORB.Setup.Access_Points.LS is

   use PolyORB.Binding_Data.GIOP.Local_Sockets;
   use PolyORB.Filters;
   use PolyORB.Filters.Slicers;
   use PolyORB.ORB;
   use PolyORB.Transport.Connected.Local_Sockets;
   use PolyORB.Log;
   use PolyORB.Local_Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.setup-access-points.ls");

   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type LS_Access_Point_Info is record
      Socket : Local_Socket_Access;
      SAP    : PolyORB.Transport.Transport_Access_Point_Access;
      PF     : PolyORB.Binding_Data.Profile_Factory_Access;
   end record;

   LS_Access_Point : LS_Access_Point_Info :=
     (Socket => null,
      SAP    => new Local_Socket_Access_Point,
      PF     => new LS_Profile_Factory);

   Sli          : aliased Slicer_Factory;
   Pro          : aliased Protocols.GIOP.Local_Sockets.Local_Sockets_Protocol;
   LS_Factories : aliased Filters.Factory_Array :=
     (0 => Sli'Access,
      1 => Pro'Access);

   ------------------------
   -- Initialize_Sockets --
   -------------------------

   procedure Initialize_Socket (DAP : in out LS_Access_Point_Info);

   procedure Initialize_Socket (DAP : in out LS_Access_Point_Info) is
      Addr : Local_Socket_Addr;
      use PolyORB.Transport;
      use PolyORB.Transport.Connected.Local_Sockets;
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.GIOP.Local_Sockets;
      Tmp : constant Local_Socket_Access := Create_Local_Socket;
   begin
      DAP.Socket := Tmp;

      if DAP.SAP = null then
         DAP.SAP := new Local_Socket_Access_Point;
      end if;

      Create (Local_Socket_Access_Point (DAP.SAP.all), DAP.Socket.all, Addr);

      if DAP.PF /= null then
         Create_Factory
           (LS_Profile_Factory (DAP.PF.all),
            DAP.SAP,
            Components.Component_Access (Setup.The_ORB));
      end if;

      pragma Debug
        (O ("Ls_Profile_Factory : Image " &
             Image (LS_Profile_Factory (DAP.PF.all))));
   end Initialize_Socket;

   ------------------------------
   -- Initialize_Access_Points --
   ------------------------------

   procedure Initialize_Access_Points;

   procedure Initialize_Access_Points is
      use PolyORB.Transport.Connected.Local_Sockets;

   begin
      Initialize_Socket (LS_Access_Point);

      Register_Access_Point
        (ORB   => The_ORB,
         TAP   => LS_Access_Point.SAP,
         Chain => LS_Factories'Access,
         PF    => LS_Access_Point.PF);
   end Initialize_Access_Points;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
     (Name      => +"access_points.lsiop",
      Conflicts => String_Lists.Empty,
      Depends   => +"orb"
      & "protocols.giop.lsiop"
      & "local_sockets",
      Provides  => String_Lists.Empty,
      Implicit  => False,
      Init      => Initialize_Access_Points'Access,
      Shutdown  => null));
end PolyORB.Setup.Access_Points.LS;
