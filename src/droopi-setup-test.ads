--  Set up a test ORB.

--  $Id$

with Droopi.ORB;
with Droopi.References;

package Droopi.Setup.Test is

   pragma Elaborate_Body;

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

   My_Ref : Droopi.References.Ref;
   --  Object reference designating the created test object.

   procedure Run_Test;
   --  Execute the test server.

end Droopi.Setup.Test;
