------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . O R B . T H R E A D _ P O O L               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
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
with PolyORB.Filters.Iface;
with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.ORB_Controller;
with PolyORB.Parameters;
with PolyORB.Setup;
with PolyORB.Task_Info;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Pool is

   use PolyORB.Filters.Iface;
   use PolyORB.Log;
   use PolyORB.ORB_Controller;
   use PolyORB.Parameters;
   use PolyORB.Task_Info;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Threads;

   package L is new PolyORB.Log.Facility_Log ("polyorb.orb.thread_pool");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   Default_Threads : constant := 4;
   --  Default number of threads in thread pool

   Minimum_Spare_Threads : Natural;
   Maximum_Spare_Threads : Natural;
   Maximum_Threads : Natural;

   type Thread_Array is array (Natural range <>) of Thread_Id;
   type Thread_Array_Access is access Thread_Array;

   The_Pool : Thread_Array_Access;
   Threads_Count : Natural := 0;
   Mutex : Mutex_Access;

   Exit_Condition_True : constant PolyORB.Types.Boolean_Ptr
     := new Boolean'(True);

   procedure Main_Thread_Pool;
   --  Main loop for threads in the pool.

   ----------------------
   -- Main_Thread_Pool --
   ----------------------

   procedure Main_Thread_Pool is
   begin
      pragma Debug (C, O ("Thread "
                       & Image (Current_Task)
                       & " is initialized"));
      Enter (Mutex);

      --  Per construction, at most The_Pool'Size tasks can enter this
      --  function, the following piece of code cannot fail.

      for J in The_Pool'Range loop
         if The_Pool (J) = Null_Thread_Id then
            The_Pool (J) := Current_Task;
            exit;
         end if;
      end loop;

      Leave (Mutex);

      PolyORB.ORB.Run (Setup.The_ORB,
                       Exit_Condition => (null, null),
                       May_Poll => True);
      pragma Debug (C, O ("Thread "
                       & Image (Current_Task)
                       & " is released"));
   end Main_Thread_Pool;

   -----------------------------
   -- Handle_Close_Connection --
   -----------------------------

   procedure Handle_Close_Connection
     (P   : access Thread_Pool_Policy;
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
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Pool_Policy;
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

      --  The newly-created channel will be monitored by general purpose ORB
      --  tasks when the binding object sends a Data_Expected message to the
      --  endpoint (which will in turn send Monitor_Endpoint to the ORB).
   end Handle_New_Server_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access Thread_Pool_Policy;
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

      --  Same comment as Handle_New_Server_Connection.
   end Handle_New_Client_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   procedure Handle_Request_Execution
     (P   : access Thread_Pool_Policy;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);

   begin
      Enter (Mutex);

      if Get_Idle_Tasks_Count (ORB.ORB_Controller) = 0
        and then Threads_Count < Maximum_Threads
      then
         Threads_Count := Threads_Count + 1;
         Leave (Mutex);
         pragma Debug (C, O ("Creating new task"));
         Create_Task (Main_Thread_Pool'Access);
      else
         Leave (Mutex);
      end if;

      pragma Debug (C, O ("Thread "
                       & Image (Current_Task)
                       & " handles request execution"));

      Run_Request (RJ);
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P         : access Thread_Pool_Policy;
      This_Task : in out PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

      package PTI  renames PolyORB.Task_Info;
      package PTCV renames PolyORB.Tasking.Condition_Variables;

   begin
      Enter (Mutex);

      if Threads_Count > Maximum_Spare_Threads then
         declare
            This_Task_Id : constant Thread_Id := Current_Task;
         begin
            for J in The_Pool'Range loop
               if The_Pool (J) = This_Task_Id then
                  pragma Debug (C, O ("Terminating Task "
                                   & Image (PTI.Id (This_Task))));

                  The_Pool (J) := Null_Thread_Id;
                  Threads_Count := Threads_Count - 1;
                  Set_Exit_Condition (This_Task, Exit_Condition_True);
                  Leave (Mutex);
                  return;
               end if;
            end loop;
         end;
      end if;

      Leave (Mutex);

      pragma Debug (C, O ("Thread "
                       & Image (PTI.Id (This_Task))
                       & " is going idle."));

      PTCV.Wait (PTI.Condition (This_Task), PTI.Mutex (This_Task));

      pragma Debug (C, O ("Thread "
                       & Image (PTI.Id (This_Task))
                       & " is leaving Idle state"));
   end Idle;

   --------------------------------------
   -- Initialize_Tasking_Policy_Thread --
   --------------------------------------

   procedure Initialize_Tasking_Policy_Access;

   procedure Initialize_Tasking_Policy_Access is
   begin
      Setup.The_Tasking_Policy := new Thread_Pool_Policy;
      Create (Mutex);
   end Initialize_Tasking_Policy_Access;

   ------------------------
   -- Initialize_Threads --
   ------------------------

   procedure Initialize_Threads;

   procedure Initialize_Threads is
   begin
      pragma Debug (C, O ("Initialize_threads : enter"));

      Minimum_Spare_Threads := Get_Conf
        ("tasking",
         "min_spare_threads",
         Default_Threads);

      Maximum_Spare_Threads := Get_Conf
        ("tasking",
         "max_spare_threads",
         Default_Threads);

      Maximum_Threads := Get_Conf
        ("tasking",
         "max_threads",
         Default_Threads);

      if not (Maximum_Threads >= Maximum_Spare_Threads
              and then Maximum_Spare_Threads >= Minimum_Spare_Threads)
      then
         raise Constraint_Error;
      end if;

      The_Pool := new Thread_Array (1 .. Maximum_Threads);

      for J in The_Pool'Range loop
         The_Pool (J) := Null_Thread_Id;
      end loop;

      pragma Debug (C, O ("Creating"
                       & Natural'Image (Minimum_Spare_Threads)
                       & " spare threads"));

      Threads_Count := Minimum_Spare_Threads;

      for J in 1 .. Minimum_Spare_Threads loop
         Create_Task (Main_Thread_Pool'Access);
      end loop;

      pragma Debug (C, O ("Initialize_threads : leave"));
   end Initialize_Threads;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb.thread_pool",
       Conflicts => +"no_tasking",
       Depends   => +"tasking.threads",
       Provides  => +"orb.tasking_policy!",
       Implicit  => False,
       Init      => Initialize_Tasking_Policy_Access'Access,
       Shutdown  => null));

   Register_Module
     (Module_Info'
      (Name      => +"orb.threads_init",
       Conflicts => +"no_tasking",
       Depends   => +"orb",
       Provides  => +"orb.tasking_policy_init",
       Implicit  => False,
       Init      => Initialize_Threads'Access,
       Shutdown  => null));

   --  Two Register_Module are needed because, on one hand, the
   --  variable Setup.The_Tasking_Policy must be initialized before
   --  ORB creation and on the other hand, the variable Setup.The_ORB
   --  must be initialized in order to run threads from the
   --  thread_pool. This breaks the circular dependecy at
   --  initialisation

end PolyORB.ORB.Thread_Pool;
