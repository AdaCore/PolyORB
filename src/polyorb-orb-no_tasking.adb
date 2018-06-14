------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . O R B . N O _ T A S K I N G                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

--  Tasking policy for the ORB core: 'No_Tasking'.

with PolyORB.Components;
with PolyORB.Filters.Iface;
with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.No_Tasking is

   use PolyORB.Filters.Iface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.no_tasking");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------------------
   -- Handle_Close_Connection --
   -----------------------------

   overriding procedure Handle_Close_Connection
     (P   : access No_Tasking;
      TE  :        Transport_Endpoint_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (TE);
      pragma Warnings (On);

   begin
      null;
   end Handle_Close_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   overriding procedure Handle_New_Client_Connection
     (P   : access No_Tasking;
      ORB :        ORB_Access;
      AC  :        Active_Connection)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P, ORB);
      pragma Warnings (On);

   begin

      pragma Debug (C, O ("New client connection"));

      Components.Emit_No_Reply (Component_Access (AC.TE),
         Connect_Confirmation'(null record));

      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.
   end Handle_New_Client_Connection;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   overriding procedure Handle_New_Server_Connection
     (P   : access No_Tasking;
      ORB :        ORB_Access;
      AC  :        Active_Connection)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P, ORB);
      pragma Warnings (On);

   begin
      pragma Debug (C, O ("New server connection"));

      Components.Emit_No_Reply (Component_Access (AC.TE),
         Connect_Indication'(null record));
      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.
   end Handle_New_Server_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   overriding procedure Handle_Request_Execution
     (P   : access No_Tasking;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class)
   is
      pragma Unreferenced (P);
      J : Job_Access := Job_Access (RJ);
   begin
      pragma Debug (C, O ("Request execution"));

      --  No tasking: execute the request in the current task.

      Run_Request (ORB, RJ.Request);
      Free (J);
   end Handle_Request_Execution;

   ------------------------
   -- Handle_Validate_TE --
   ------------------------

   overriding procedure Handle_Validate_TE
     (P  : access No_Tasking;
      TE : Transport_Endpoint_Access)
   is
      pragma Unreferenced (P);
   begin
      Transport.Check_Validity (TE);
   end Handle_Validate_TE;

   ----------
   -- Idle --
   ----------

   overriding procedure Idle
     (P         : access No_Tasking;
      This_Task : PTI.Task_Info_Access;
      ORB       : ORB_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (This_Task);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

   begin
      pragma Debug (C, O ("Dead lock detected !"));

      raise Program_Error;
      --  When there is no tasking, the (only) task in the
      --  application may not go idle, since this would
      --  block the whole system forever.
   end Idle;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Setup.The_Tasking_Policy := new No_Tasking;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb.no_tasking",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"orb.tasking_policy!",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB.No_Tasking;
