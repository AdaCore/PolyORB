------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O L Y O R B . O R B . N O _ T A S K I N G                --
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

--  Tasking policy for the ORB core: 'No_Tasking'.

--  $Id$

with PolyORB.Components;
with PolyORB.Initialization;
with PolyORB.Filters.Interface;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.No_Tasking is

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.no_tasking");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   ------------------------------------
   -- Handle_Close_Server_Connection --
   ------------------------------------

   procedure Handle_Close_Server_Connection
     (P   : access No_Tasking;
      TE  : Transport_Endpoint_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (TE);
      pragma Warnings (On);
      null;
   end Handle_Close_Server_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O ("No_Tasking: new client connection"));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Confirmation'(null record));

      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.
   end Handle_New_Client_Connection;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O ("No_Tasking: new server connection"));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));
      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.
   end Handle_New_Server_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   procedure Handle_Request_Execution
     (P   : access No_Tasking;
      ORB : ORB_Access;
      RJ  : access Request_Job'Class) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      pragma Debug (O ("No_Tasking: request execution"));

      Run_Request (RJ);
      --  No tasking: execute the request in the current task.
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle (P : access No_Tasking; ORB : ORB_Access) is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      pragma Debug (O ("No_Tasking: Idle -> Program error is raised"));
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

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access No_Tasking;
      ORB : ORB_Access;
      Msg : Message'Class)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      Emit_No_Reply (Component_Access (ORB), Msg);
   end Queue_Request_To_Handler;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"orb.no_tasking",
       Conflicts => Empty,
       Depends => +"soft_links",
       Provides => +"orb.tasking_policy",
       Init => Initialize'Access));
end PolyORB.ORB.No_Tasking;
