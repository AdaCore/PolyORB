------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . O R B . T H R E A D _ P O O L               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
with PolyORB.Configuration;
with PolyORB.Initialization;
with PolyORB.Filters.Interface;
with PolyORB.Jobs;
with PolyORB.Log;
with PolyORB.Setup;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Pool is

   ------------------------
   -- Local declarations --
   ------------------------

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Soft_Links;
   use PolyORB.Components;
   use PolyORB.Configuration;

   package L is new PolyORB.Log.Facility_Log ("polyorb.orb.thread_pool");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   A_ORB : ORB_Access := null;
   --  global variables for thread initialisation

   Thread_Init_Watcher    : Watcher_Access := null;
   Thread_Init_Version_Id : Version_Id;
   --  used at thread initialisation

   Initialized : Boolean := False;
   --  indicates if initialisation has been done yet

   Default_Threads : constant := 4;
   --  default number of threads in thread pool
   --  XXX should check compatibility with ravenscar, which also defines
   --  a number of threads ...

   procedure Main_Thread_Pool;
   --  Main procedure for threads in the pool

   ----------------------
   -- Main_Thread_Pool --
   ----------------------

   procedure Main_Thread_Pool
   is
      ORB : ORB_Access := null;
   begin
      ORB := A_ORB;
      Update (Thread_Init_Watcher);
      Run (ORB);
   end Main_Thread_Pool;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      C   : Active_Connection)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O ("Thread_Pool: new server connection"));
      if Initialized = False then
         Initialize
           (Get_Conf ("tasking", "polyorb.orb.thread_pool.threads",
                      Default_Threads),
            ORB);
      end if;
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));

   --  The newly-created channel will be monitored
   --  by general-purpose ORB tasks.
   end Handle_New_Server_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      C   : Active_Connection)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O ("Thread_Pool: new client connection"));
      if Initialized = False then
         Initialize
           (Get_Conf ("tasking", "polyorb.orb.thread_pool.threads",
                      Default_Threads),
            ORB);
      end if;
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Confirmation'(null record));

   --  The newly-created channel will be monitored
   --  by general-purpose ORB tasks.
   end Handle_New_Client_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   procedure Handle_Request_Execution
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      RJ  : access Jobs.Job'Class)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O ("Thread_Pool: thread "
                       & Image (Current_Task)
                       & "handle request execution"));
      if Initialized = False then
         Initialize
           (Get_Conf ("tasking", "polyorb.orb.thread_pool.threads",
                      Default_Threads),
            ORB);
      end if;
      Jobs.Run (RJ);
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P : access Thread_Pool_Policy;
      ORB : ORB_Access)
   is
      use PolyORB.Soft_Links;
      V : Version_Id;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      pragma Debug (O ("Going idle."));

      Lookup (ORB.Idle_Tasks, V);
      pragma Debug (O ("Version_Id :" & Integer'Image (Integer (V))));
      Differ (ORB.Idle_Tasks, V);
      pragma Debug (O ("Stopping idle."));
      --  XXXXX ???
      --  raise Program_Error;
      --  When in Thread_Pool mode, threads should not be allowed
      --  to go idle, but should be blocked when the request queue
      --  is empty. XXX *But* application threads that are not part
      --  of the thread pool may need to idle!
   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      Msg : Message'Class)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      if Initialized = False then
         Initialize
           (Get_Conf ("tasking", "polyorb.orb.thread_pool.threads",
                      Default_Threads),
            ORB);
      end if;
      Emit_No_Reply (Component_Access (ORB), Msg);
   end Queue_Request_To_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Number_Of_Threads : Positive;
      ORB               : ORB_Access)
   is
   begin
      pragma Debug (O ("Initialize : enter"));
      Initialized := True;
      A_ORB := ORB;
      for J in 1 .. Number_Of_Threads loop
         Create_Task (Main_Thread_Pool'Access);
         Differ (Thread_Init_Watcher, Thread_Init_Version_Id);
         Lookup (Thread_Init_Watcher, Thread_Init_Version_Id);
      end loop;
   end Initialize;

   ---------------------
   -- Auto_Initialize --
   ---------------------
   procedure Auto_Initialize;

   procedure Auto_Initialize is
      use PolyORB.Configuration;
   begin
      Setup.The_Tasking_Policy := new Thread_Pool_Policy;
      Create (Thread_Init_Watcher);
      Lookup (Thread_Init_Watcher, Thread_Init_Version_Id);
   end Auto_Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"orb.thread_pool",
       Conflicts => +"no_tasking",
       Depends => +"soft_links",
       Provides => +"orb.tasking_policy",
       Init => Auto_Initialize'Access));
end PolyORB.ORB.Thread_Pool;
