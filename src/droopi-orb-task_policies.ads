--  Tasking policy for the ORB core: 'No_Tasking'.

--  $Id$

package Droopi.ORB.Task_Policies is

   pragma Elaborate_Body;

   ---------------------------------------------------------
   -- Simple policy for configuration without any tasking --
   ---------------------------------------------------------

   --  This policy may be used for the creation of a low-profile
   --  ORB that does not depend on the Ada tasking runtime library.
   --  It is suitable for use in a node that contains only an
   --  environment task.

   type No_Tasking is new Tasking_Policy_Type with private;

   procedure Handle_New_Server_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_New_Client_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_Request_Execution
     (P   : access No_Tasking;
      ORB : ORB_Access;
      RJ  : Jobs.Job_Access);

   procedure Idle (P : access No_Tasking; ORB : ORB_Access);

   procedure Queue_Request_To_Handler
     (P   : access No_Tasking;
      ORB : ORB_Access;
      Msg : Message'Class);

private

   type No_Tasking is new Tasking_Policy_Type with null record;

end Droopi.ORB.Task_Policies;
