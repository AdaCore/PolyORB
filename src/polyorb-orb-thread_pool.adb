------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . O R B . T H R E A D _ P O O L               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2004 Free Software Foundation, Inc.           --
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
with PolyORB.Filters.Interface;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Setup;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Pool is

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Parameters;
   use PolyORB.Tasking.Threads;

   package L is new PolyORB.Log.Facility_Log ("polyorb.orb.thread_pool");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Default_Threads : constant := 4;
   --  Default number of threads in thread pool
   --  XXX should check compatibility with ravenscar, which also defines
   --  a number of threads ...

   procedure Main_Thread_Pool;
   --  Main loop for threads in the pool.

   ----------------------
   -- Main_Thread_Pool --
   ----------------------

   procedure Main_Thread_Pool is
   begin
      pragma Debug (O ("Thread "
                       & Image (Current_Task)
                       & " is initialized"));
      PolyORB.ORB.Run (Setup.The_ORB, May_Poll => True);
      pragma Debug (O ("Thread "
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
      C   :        Active_Connection)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P, ORB);
      pragma Warnings (On);

   begin
      pragma Debug (O ("New server connection"));

      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));

      --  The newly-created channel will be monitored by
      --  general-purpose ORB tasks when the binding object sends a
      --  Data_Expected message to the endpoint (which will in turn
      --  send Monitor_Endpoint to the ORB).
   end Handle_New_Server_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access Thread_Pool_Policy;
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
      pragma Unreferenced (ORB);
      pragma Warnings (On);

   begin
      pragma Debug (O ("Thread "
                       & Image (Current_Task)
                       & " handles request execution"));

      Run_Request (RJ);
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P         : access Thread_Pool_Policy;
      This_Task :        PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

      package PTI  renames PolyORB.Task_Info;
      package PTCV renames PolyORB.Tasking.Condition_Variables;

   begin
      pragma Debug (O ("Thread "
                       & Image (PTI.Id (This_Task))
                       & " is going idle."));

      PTCV.Wait (PTI.Condition (This_Task), PTI.Mutex (This_Task));

      pragma Debug (O ("Thread "
                       & Image (PTI.Id (This_Task))
                       & " is leaving Idle state"));
   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access Thread_Pool_Policy;
      ORB :        ORB_Access;
      Msg :        Message'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);

   begin
      Emit_No_Reply (Component_Access (ORB), Msg);
   end Queue_Request_To_Handler;

   --------------------------------------
   -- Initialize_Tasking_Policy_Thread --
   --------------------------------------

   procedure Initialize_Tasking_Policy_Access;

   procedure Initialize_Tasking_Policy_Access is
   begin
      Setup.The_Tasking_Policy := new Thread_Pool_Policy;
   end Initialize_Tasking_Policy_Access;

   ------------------------
   -- Initialize_Threads --
   ------------------------

   procedure Initialize_Threads;

   procedure Initialize_Threads
   is
      Number_Of_Threads : Positive;

   begin
      pragma Debug (O ("Initialize_threads : enter"));

      Number_Of_Threads := Get_Conf
        ("tasking",
         "polyorb.orb.thread_pool.threads",
         Default_Threads);

      for J in 1 .. Number_Of_Threads loop
         Create_Task (Main_Thread_Pool'Access);
      end loop;

      pragma Debug (O ("Initialize_threads : leave"));
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
       Provides  => +"orb.tasking_policy",
       Implicit  => False,
       Init      => Initialize_Tasking_Policy_Access'Access));

   Register_Module
     (Module_Info'
      (Name      => +"orb.threads_init",
       Conflicts => +"no_tasking",
       Depends   => +"orb",
       Provides  => +"orb.tasking_policy_init",
       Implicit  => False,
       Init      => Initialize_Threads'Access));

   --  Two Register_Module are needed because, on one hand, the
   --  variable Setup.The_Tasking_Policy must be initialized before
   --  ORB creation and on the other hand, the variable Setup.The_ORB
   --  must be initialized in order to run threads from the
   --  thread_pool. This breaks the circular dependecy at
   --  initialisation

end PolyORB.ORB.Thread_Pool;
