--  Set up a test ORB.

--  $Id$

with Droopi.ORB;

package Droopi.Setup.Test is

   type Parameterless_Procedure is access procedure;

   procedure Initialize_Test_Server
     (SL_Init : Parameterless_Procedure;
      TP : ORB.Tasking_Policy_Access);
   --  Initialize middleware subsystems and create ORB.
   --  SL_Init must initialize one of the Soft_Links implementations.
   --  TP must be the chosen ORB tasking policy.

   procedure Initialize_Test_Access_Points;
   --  Create the communication endpoints for the test server.

   procedure Initialize_Test_Object;
   --  Create the test object implementation.

   procedure Run_Test;
   --  Execute the test server.

end Droopi.Setup.Test;
