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

with Ada.Exceptions;

with PolyORB.Components;
with PolyORB.Configuration;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with PolyORB.Filters.Interface;
with PolyORB.Jobs;
with PolyORB.Locked_Queue;
pragma Elaborate_All (PolyORB.Locked_Queue);
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Setup;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Pool is

   --  The tread pool works in the following manner :
   --
   --  Initialize spawns a fixed number of pool threads that will
   --  execute client requests.
   --
   --  Whenever a request comes, it is enqueued in The_Request_Queue.
   --
   --  Whenever a pool thread has nothing to do, it gets the first
   --  request in The_Request_Queue and executes it.

   ------------------------
   -- Local declarations --
   ------------------------

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Soft_Links;
   use PolyORB.Components;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_pool");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   task type Pool_Thread is
      entry Start (N : in Natural);
      pragma Storage_Size (131072);
   end Pool_Thread;
   type Pool_Thread_Access is access Pool_Thread;

   type Thread_Array is array (Integer range <>) of Pool_Thread;
   type Thread_Array_Access is access Thread_Array;

   The_Thread_Pool : Thread_Array_Access := null;

   type Request_Info is record
      Job : Jobs.Job_Access;
   end record;

   package Request_Queue is new Locked_Queue (Request_Info);

   The_Request_Queue : Request_Queue.Queue;

   -----------------
   -- Pool_Thread --
   -----------------

   task body Pool_Thread
   is
      Request : Request_Info;
      Number  : Natural;
   begin
      accept Start (N : in Natural) do
         Number := N;
         pragma Debug (O ("Thread"  & Integer'Image (Number) & " starts"));
      end Start;
      loop
         Request_Queue.Get_Head (The_Request_Queue, Request);

         pragma Debug (O ("Thread Pool : Thread"
                          & Integer'Image (Number)
                          & " is executing request"));

         Jobs.Run (Request.Job);
         Jobs.Free (Request.Job);

         pragma Debug (O ("Thread Pool : Thread"
                          & Integer'Image (Number)
                          & " has executed request"));
      end loop;
   exception
      when E : others =>
         pragma Debug (O ("Thread_Pool: Thread" & Number'Img
                          & " caught an exception:"));
         pragma Debug (O (Ada.Exceptions.Exception_Information
                          (E)));
         null;
   end Pool_Thread;

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
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      pragma Debug (O ("Thread_Pool: handle request execution"));
      Request_Queue.Add
        (The_Request_Queue,
         Request_Info'(Job => PolyORB.ORB.Duplicate_Request_Job (RJ)));
      --  Must copy now, because the caller will free RJ soon.
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
      Differ (ORB.Idle_Tasks, V);

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
      Emit_No_Reply (Component_Access (ORB), Msg);
   end Queue_Request_To_Handler;

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;
   Default_Threads : constant := 4;
   Default_Queue_Size : constant := 10;

   procedure Initialize
     (Number_Of_Threads : Positive;
      Queue_Size        : Positive)
   is
      Dummy_Task : Pool_Thread_Access;
   begin
      pragma Debug (O ("Initialize : enter"));
      if Initialized then
         pragma Debug (O ("Thread_Pool: already initialized!"));
         return;
      end if;
      Initialized := True;
      The_Thread_Pool := new Thread_Array (1 .. Number_Of_Threads);

      Request_Queue.Create (The_Request_Queue, Queue_Size);

      for J in The_Thread_Pool'Range loop
         Dummy_Task := new Pool_Thread;
         Dummy_Task.Start (J);
      end loop;
   end Initialize;

   procedure Auto_Initialize;

   procedure Auto_Initialize is
      use PolyORB.Configuration;
   begin
      Setup.The_Tasking_Policy := new Thread_Pool_Policy;

      Initialize
        (Get_Conf ("tasking", "polyorb.orb.thread_pool.threads", 4),
         Get_Conf ("tasking", "polyorb.orb.thread_pool.queue_size", 10));
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
