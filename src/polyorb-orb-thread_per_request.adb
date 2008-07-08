------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ R E Q U E S T        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Filters.Iface;
with PolyORB.Initialization;

with PolyORB.Jobs;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Per_Request is

   ------------------------
   -- Local declarations --
   ------------------------

   use PolyORB.Asynch_Ev;
   use PolyORB.Filters;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Threads;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_per_request");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   type Request_Runnable is new Runnable with record
      A_Job : Jobs.Job_Access;
   end record;

   type Request_Runnable_Controller is
     new Runnable_Controller with null record;

   procedure Run (R : access Request_Runnable);

   procedure Initialize;

   -----------------------------
   -- Handle_Close_Connection --
   -----------------------------

   procedure Handle_Close_Connection
     (P   : access Thread_Per_Request_Policy;
      TE  :        Transport_Endpoint_Access)
   is
      pragma Unreferenced (P, TE);
   begin
      null;
   end Handle_Close_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection)
   is
      pragma Unreferenced (P, ORB);
   begin
      pragma Debug (O ("New client connection"));

      Components.Emit_No_Reply (Component_Access (AC.TE),
         Connect_Confirmation'(null record));
   end Handle_New_Client_Connection;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Request_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection)
   is
      pragma Unreferenced (P, ORB);
   begin
      pragma Debug (O ("New server connection. "));

      Components.Emit_No_Reply (Component_Access (AC.TE),
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
      pragma Unreferenced (P, ORB);

      R : constant Runnable_Access := new Request_Runnable;

      T : Thread_Access;
      pragma Unreferenced (T);
      --  T is assigned but never read

   begin
      pragma Debug (O ("Handle_Request_Execution : Run Job"));
      Request_Runnable (R.all).A_Job := PolyORB.ORB.Duplicate_Request_Job (RJ);

      T := Run_In_Task
        (Get_Thread_Factory,
         R => R,
         C => new Request_Runnable_Controller);
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P         : access Thread_Per_Request_Policy;
      This_Task : in out PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access)
   is
      pragma Unreferenced (P, ORB);

      package PTI  renames PolyORB.Task_Info;
   begin

      --  In Thread_Per_Request policy, only one task is executing ORB.Run.
      --  However, it can be set to idle while another thread modifies
      --  ORB internals.

      pragma Debug (O ("Thread "
                       & Image (PTI.Id (This_Task))
                       & " is going idle."));

      Wait (PTI.Condition (This_Task), PTI.Mutex (This_Task));

      pragma Debug (O ("Thread "
                       & Image (PTI.Id (This_Task))
                       & " is leaving Idle state"));
   end Idle;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Setup.The_Tasking_Policy := new Thread_Per_Request_Policy;
   end Initialize;

   ---------
   -- Run --
   ---------

   procedure Run (R : access Request_Runnable) is
   begin

      --  Running Job

      pragma Debug (O ("Thread " & Image (Current_Task)
                       & " is executing a job"));

      Run_Request (Request_Job (R.A_Job.all)'Access);

      --  Job Finalization

      Jobs.Free (R.A_Job);

      pragma Debug (O ("Thread " & Image (Current_Task)
                       & " has executed and destroyed a job"));
   end Run;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb.thread_per_request",
       Conflicts => +"no_tasking",
       Depends   => +"tasking.condition_variables",
       Provides  => +"orb.tasking_policy",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB.Thread_Per_Request;
