--  $Id$

with Unchecked_Deallocation;

with Droopi.Log;

with Droopi.Locks;
--  ??? : Since we use only writers locks, we could use a lighter
--        implementation.

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

   use Droopi.Log;
   use Droopi.Locks;

   package L is new Droopi.Log.Facility_Log
     ("droopi.orb.tasking_policies");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   task type Pool_Thread is
      entry Start;
   end Pool_Thread;
   type Pool_Thread_Access is access Pool_Thread;

   type Thread_Array is array (Integer range <>) of Pool_Thread;
   type Thread_Array_Access is access Thread_Array;

   The_Thread_Pool : Thread_Array_Access := null;

   type Request_Info is record
      Job : Jobs.Job_Access;
   end record;

   type Request_Queue_Node;
   type Request_Queue_Node_Access is access Request_Queue_Node;

   type Request_Queue_Node is record
      Request : Request_Info;
      Next    : Request_Queue_Node_Access;
   end record;

   procedure Free is new Unchecked_Deallocation
     (Request_Queue_Node, Request_Queue_Node_Access);

   type Request_Queue is record
      Max_Count  : Positive;

      State_Lock : Rw_Lock_Access;
      --  This locks the global state of the queue, and should be
      --  taken when modifying First, Last and Count fields.

      Full_Lock  : Rw_Lock_Access;
      --  This lock is taken when the queue is full.

      Empty_Lock : Rw_Lock_Access;
      --  This lock is taken when the queue is empty.

      First      : Request_Queue_Node_Access := null;
      Last       : Request_Queue_Node_Access := null;
      Count      : Natural := 0;
   end record;

   type Request_Queue_Access is access Request_Queue;

   The_Request_Queue : Request_Queue_Access := null;

   procedure Add
     (Queue   : Request_Queue_Access;
      Element : in Request_Info);
   --  Appends an element to the end of the queue.
   --  This call is blocking when the queue is full.
   --
   --  ??? : This is the only function that needs to be modified when
   --        adding the notion of priority to tasks : request should be
   --        inserted in the queue with regard to their priority and
   --        not necessarily at the end.

   procedure Get_Head
     (Queue   : Request_Queue_Access;
      Element : out Request_Info);
   --  Removes the first element in the queue and returns it.
   --  This call is blocking when the queue is empty.

   --------------
   -- Get_Head --
   --------------

   procedure Get_Head
     (Queue   : Request_Queue_Access;
      Element : out Request_Info)
   is
   begin
      Lock_W (Queue.Empty_Lock);

      --  When execution reaches this, necessarily Queue.First /= null.

      Lock_W (Queue.State_Lock);
      declare
         Old_First : Request_Queue_Node_Access := Queue.First;
      begin
         Element := Queue.First.Request;
         Queue.First := Queue.First.Next;
         Free (Old_First);
      end;

      Queue.Count := Queue.Count - 1;
      Unlock_W (Queue.State_Lock);

      --  When execution reaches this, necessarily the queue is not full.

      Unlock_W (Queue.Full_Lock);

      if Queue.Count > 0 then
         Unlock_W (Queue.Empty_Lock);
      end if;
   end Get_Head;

   ---------
   -- Add --
   ---------

   procedure Add
     (Queue   : Request_Queue_Access;
      Element : in Request_Info)
   is
   begin
      Lock_W (Queue.Full_Lock);

      Lock_W (Queue.State_Lock);
      if Queue.Last = null then
         Queue.Last := new Request_Queue_Node'
           (Request => Element,
            Next    => null);
      else
         Queue.Last.Next := new Request_Queue_Node'
           (Request => Element,
            Next    => null);
         Queue.Last := Queue.Last.Next;
      end if;

      Queue.Count := Queue.Count + 1;
      Unlock_W (Queue.State_Lock);

      --  When execution reaches this, necessarily the queue is not empty.
      Unlock_W (Queue.Empty_Lock);

      if Queue.Count < Queue.Max_Count then
         Unlock_W (Queue.Full_Lock);
      end if;
   end Add;

   -----------------
   -- Pool_Thread --
   -----------------

   task body Pool_Thread
   is
      Request : Request_Info;
   begin
      accept Start;
      loop
         Get_Head (The_Request_Queue, Request);
         pragma Debug (O ("Thread Pool : Thread is executing request"));

         Jobs.Run (Request.Job);
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
      null;
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
      null;
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
      Add (The_Request_Queue, Request_Info'(Job => RJ));
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P : access Thread_Pool_Policy;
      ORB : ORB_Access)
   is
   begin
      pragma Debug (O ("Thread_Pool: Idle (BAD BAD!)"));
      raise Program_Error;
      --  When in Thread_Pool mode, threads should not be allowed
      --  to go idle, but should be blocked when the request queue is empty.
   end Idle;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Number_Of_Threads : Positive;
      Queue_Size        : Positive)
   is
      Dummy_Task : Pool_Thread_Access;
   begin
      pragma Debug (O ("Thread_Pool : spawning pool."));
      The_Thread_Pool := new Thread_Array (1 .. Number_Of_Threads);
      The_Request_Queue := new Request_Queue;
      The_Request_Queue.Max_Count := Queue_Size;

      Create (The_Request_Queue.State_Lock);
      Create (The_Request_Queue.Full_Lock);
      Create (The_Request_Queue.Empty_Lock);

      Lock_W (The_Request_Queue.Empty_Lock);

      for J in The_Thread_Pool'Range loop
         Dummy_Task := new Pool_Thread;
         Dummy_Task.Start;
      end loop;

   end Initialize;

end Droopi.ORB.Thread_Pool;
