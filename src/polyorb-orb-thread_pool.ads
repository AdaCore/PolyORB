--  Implementation of thread pool architecture

--  $Id$

package PolyORB.ORB.Thread_Pool is

   pragma Elaborate_Body;

   ----------------------------------------------------
   -- Implementation of a thread-pool tasking policy --
   ----------------------------------------------------

   type Thread_Pool_Policy is new Tasking_Policy_Type with private;

   procedure Handle_New_Server_Connection
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_New_Client_Connection
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_Request_Execution
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      RJ  : access Jobs.Job'Class);

   procedure Idle
     (P : access Thread_Pool_Policy;
      ORB : ORB_Access);

   procedure Queue_Request_To_Handler
     (P   : access Thread_Pool_Policy;
      ORB : ORB_Access;
      Msg : Message'Class);

   procedure Initialize
     (Number_Of_Threads : Positive;
      Queue_Size        : Positive);
   --  This function must be called once before any other call in this package.
   --  Number_Of_Threads indicates how many threads will be created to deal
   --  with client requests.
   --  Queue_Size indicates how many requests can be queued while waiting for
   --  a thread to become available.
   --  When this queue is full, calls to Handle_Request_Execution become
   --  blocking.
   --  See package body for more information.

private

   type Thread_Pool_Policy is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Thread_Pool;
