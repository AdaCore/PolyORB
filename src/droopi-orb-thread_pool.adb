--  $Id$

with Droopi.Log;
with Droopi.Jobs;
with Droopi.Components;
with Droopi.Filters.Interface;

with Locked_Queue;

pragma Elaborate_All (Locked_Queue);
pragma Elaborate_All (Droopi.Log);

package body Droopi.ORB.Thread_Pool is

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

   use Droopi.Components;
   use Droopi.Filters.Interface;
   use Droopi.Log;
   use Droopi.Soft_Links;
   use Droopi.Components;

   package L is new Droopi.Log.Facility_Log
     ("droopi.orb.thread_pool");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   task type Pool_Thread is
      entry Start (N : in Natural);
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

         --  That's when we free jobs.
         Jobs.Free (Request.Job);

         pragma Debug (O ("Thread Pool : Thread"
                          & Integer'Image (Number)
                          & " has executed request"));
      end loop;
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
      RJ  : Jobs.Job_Access)
   is
   begin
      pragma Debug (O ("Thread_Pool: handle request execution"));
      Request_Queue.Add (The_Request_Queue, Request_Info'(Job => RJ));
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P : access Thread_Pool_Policy;
      ORB : ORB_Access)
   is
   begin
      pragma Debug (O ("Idle : going Idle (BAD BAD!)"));
      raise Program_Error;
      --  When in Thread_Pool mode, threads should not be allowed
      --  to go idle, but should be blocked when the request queue is empty.
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
      Emit_No_Reply
        (Component_Access (ORB),
         Msg);
   end Queue_Request_To_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Number_Of_Threads : Positive;
      Queue_Size        : Positive)
   is
      Dummy_Task : Pool_Thread_Access;
   begin
      pragma Debug (O ("Initialize : enter"));
      The_Thread_Pool := new Thread_Array (1 .. Number_Of_Threads);

      Request_Queue.Create (The_Request_Queue, Queue_Size);

      for J in The_Thread_Pool'Range loop
         Dummy_Task := new Pool_Thread;
         Dummy_Task.Start (J);
      end loop;
   end Initialize;

end Droopi.ORB.Thread_Pool;
