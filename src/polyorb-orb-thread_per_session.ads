package PolyORB.ORB.Thread_Per_Session is

   pragma Elaborate_Body;

   -----------------------------------------------------------
   -- Implementation of a thread-per-session tasking policy --
   -----------------------------------------------------------

   type Thread_Per_Session_Policy is new Tasking_Policy_Type with private;

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_Request_Execution
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      RJ  : Jobs.Job_Access);

   procedure Idle
     (P : access Thread_Per_Session_Policy;
      ORB : ORB_Access);

   procedure Queue_Request_To_Handler
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      Msg : Message'Class);

private

   type Thread_Per_Session_Policy is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Thread_Per_Session;

