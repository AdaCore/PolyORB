--  Tasking policies for the ORB core.

--  $Id$

with Droopi.Requests;

package Droopi.ORB.Task_Policies is

   ----------------------------------
   -- Abstract tasking policy type --
   ----------------------------------

   --  A tasking policy is a set of associations between
   --  certain events and the resources used to process them.
   --  These associations take the form of subprograms
   --  that take the event as input, create a job for
   --  its processing, and either create a new task for
   --  the execution of this job, or schedule it for execution
   --  by a general-purpose ORB task.

   type Tasking_Policy is abstract tagged limited private;
   type Tasking_Policy_Access is access all Tasking_Policy'Class;

   procedure Handle_New_Connection
     (P   : access Tasking_Policy;
      ORB : ORB_Access;
      AS  : Active_Socket)
     is abstract;
   --  Create the necessary processing resources for newly-created
   --  communication channel C, and start dialog.

   procedure Handle_Request
     (P   : access Tasking_Policy;
      ORB : ORB_Access;
      R   : Droopi.Requests.Request)
     is abstract;
   --  Create the necessary processing resources for the execution
   --  of request R, and start this execution.

   ---------------------------------------------------------
   -- Simple policy for configuration without any tasking --
   ---------------------------------------------------------

   --  This policy may be used for the creation of a low-profile
   --  ORB that does not depend on the Ada tasking runtime library.
   --  It is suitable for use in a node that contains only an
   --  environment task.

   type No_Tasking is new Tasking_Policy with private;

   procedure Handle_New_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      AS  : Active_Socket);

   procedure Handle_Request
     (P   : access No_Tasking;
      ORB : ORB_Access;
      R   : Droopi.Requests.Request);

private

   type Tasking_Policy is abstract tagged limited null record;

   type No_Tasking is new Tasking_Policy with null record;

end Droopi.ORB.Task_Policies;
