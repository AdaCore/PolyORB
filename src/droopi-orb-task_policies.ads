--  Tasking policies for the ORB core.

--  $Id$

with Droopi.Requests;

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

   procedure Handle_New_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection);

   procedure Handle_Request
     (P   : access No_Tasking;
      ORB : ORB_Access;
      R   : Droopi.Requests.Request);

private

   type No_Tasking is new Tasking_Policy_Type with null record;

end Droopi.ORB.Task_Policies;
