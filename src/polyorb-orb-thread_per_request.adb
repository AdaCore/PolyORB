------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ R E Q U E S T        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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

--  $Id$

with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Jobs;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
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
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;
   use PolyORB.Transport;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_per_request");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   A_Job : Jobs.Job_Access;
   --  This variables are used to initialized the threads local variables.
   --  They are used to replaced the accept statement
   --  In this policy we can assume that there is only one task which
   --  executes ORB.Run. There is therefore no risk of inconsistent
   --  interleaving of reads and writes to this variable.

   Job_Mutex : Tasking.Mutexes.Mutex_Access;
   Job_Taken : Condition_Access;

   procedure Request_Thread;
   --  Main loop executed by thread processing a request.

   ------------------------------------
   -- Handle_Close_Server_Connection --
   ------------------------------------

   procedure Handle_Close_Server_Connection
     (P   : access Thread_Per_Request_Policy;
      TE  :        Transport_Endpoint_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (TE);
      pragma Warnings (On);

   begin
      null;
   end Handle_Close_Server_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      C   :        Active_Connection)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P, ORB);
      pragma Warnings (On);

   begin
      pragma Debug (O ("New client connection"));

      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Confirmation'(null record));
   end Handle_New_Client_Connection;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      C   :        Active_Connection)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P, ORB);
      pragma Warnings (On);

   begin
      pragma Debug (O ("New server connection. "));

      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));
   end Handle_New_Server_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   procedure Handle_Request_Execution
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Handle_Request_Execution : Run Job"));

      Enter (Job_Mutex);
      A_Job := PolyORB.ORB.Duplicate_Request_Job (RJ);
      Create_Task (Request_Thread'Access);
      Wait (Job_Taken, Job_Mutex);
      Leave (Job_Mutex);
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P         : access Thread_Per_Request_Policy;
      This_Task :        PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (This_Task);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

   begin
      raise Program_Error;

      --  In Thread_Per_Request policy, only one task is executing
      --  ORB.Run. Thus blocking the task is an erroneous execution.
   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      Msg :        Message'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);

   begin
      Emit_No_Reply (Component_Access (ORB), Msg);
   end Queue_Request_To_Handler;

   --------------------
   -- Request_Thread --
   --------------------

   procedure Request_Thread
   is
      Job : Jobs.Job_Access;
   begin
      --  Job Initialization.
      Job := A_Job;
      Enter (Job_Mutex);
      Signal (Job_Taken);
      Leave (Job_Mutex);

      --  Running Job.
      pragma Debug (O ("Thread "
        & Image (Current_Task)
                       & " is executing a job"));

      Run_Request (Request_Job (Job.all)'Access);

      --  Job Finalization.
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
      Create (Job_Mutex);
      Create (Job_Taken);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb.thread_per_request",
       Conflicts => +"no_tasking",
       Depends   => +"tasking.mutexes"
       & "tasking.condition_variables",
       Provides  => +"orb.tasking_policy",
       Implicit  => False,
       Init      => Initialize'Access));
end PolyORB.ORB.Thread_Per_Request;
