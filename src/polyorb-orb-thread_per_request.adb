------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ R E Q U E S T        --
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

--  $Id$

with PolyORB.Components;
with PolyORB.Initialization;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Jobs;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Per_Request is

   ------------------------
   -- Local declarations --
   ------------------------

   use PolyORB.Asynch_Ev;
   use PolyORB.Components;
   use PolyORB.Filters;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Tasking.Threads;
   use PolyORB.Tasking.Watchers;
   use PolyORB.Transport;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_per_request");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   A_Job : Jobs.Job_Access := null;
   --  This variables are used to initialized the threads local variables.
   --  They are used to replaced the accept statement
   --  XXX There is nothing to prevent inconsistent interleaving
   --      of writes and reads to this variable!!!
   --  In this policy we can assert that there is only one task which executes
   --  ORB.Run so the assertion XXX is wrong

   Thread_Init_Watcher    : Watcher_Access := null;
   Thread_Init_Version_Id : Version_Id;
   --  This Watcher and his associated Version Id are used during
   --  the initialisation of a thread.

   procedure Request_Thread;
   --  This procedure is a parameterless procedure used as
   --  the body of the threads

   ------------------------------------
   -- Handle_Close_Server_Connection --
   ------------------------------------

   procedure Handle_Close_Server_Connection
     (P   : access Thread_Per_Request_Policy;
      TE  :        Transport_Endpoint_Access)
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
     (P   : access Thread_Per_Request_Policy;
      ORB : ORB_Access;
      C   : Active_Connection)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O (" new client connection"));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Confirmation'(null record));
   end Handle_New_Client_Connection;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Request_Policy;
      ORB : ORB_Access;
      C   : Active_Connection)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O (" new server connection. "));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));
   end Handle_New_Server_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   procedure Handle_Request_Execution
     (P   : access Thread_Per_Request_Policy;
      ORB : ORB_Access;
      RJ  : access Request_Job'Class)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      pragma Debug (O ("Handle_Request_Execution : Run Job"));

      A_Job := PolyORB.ORB.Duplicate_Request_Job (RJ);
      Create_Task (Request_Thread'Access);
      Differ (Thread_Init_Watcher, Thread_Init_Version_Id);
      Lookup (Thread_Init_Watcher, Thread_Init_Version_Id);
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P : access Thread_Per_Request_Policy;
      ORB : ORB_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      raise Program_Error;
      --  In Thread_Per_Request policy, only one task is executing ORB.Run
      --  So this task shouldn't go idle, since this would block the system
      --  forever
   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access Thread_Per_Request_Policy;
      ORB : ORB_Access;
      Msg : Message'Class)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      Emit_No_Reply (Component_Access (ORB), Msg);
   end Queue_Request_To_Handler;

   --------------------
   -- Request_Thread --
   --------------------

   procedure Request_Thread
   is
      Job : Jobs.Job_Access;
   begin
      --  Initialisation
      Job := A_Job;
      Update (Thread_Init_Watcher);
      pragma Debug (O ("Thread "
        & Image (Current_Task)
        & " is executing a job"));

      Run_Request (Request_Job (Job.all)'Access);

      --  Job is executed
      Jobs.Free (Job);
      pragma Debug (O ("Thread "
        & Image (Current_Task)
        & " has executed and destroyed a job"));
   end Request_Thread;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Setup.The_Tasking_Policy := new Thread_Per_Request_Policy;
      Create (Thread_Init_Watcher);
      Lookup (Thread_Init_Watcher, Thread_Init_Version_Id);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"orb.thread_per_request",
       Conflicts => +"no_tasking",
       Depends => +"soft_links",
       Provides => +"orb.tasking_policy",
       Init => Initialize'Access));
end PolyORB.ORB.Thread_Per_Request;
