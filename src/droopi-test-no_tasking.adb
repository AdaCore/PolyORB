--  Setup a test server with no tasking at all.

--  $Id$

with Droopi.Setup.Test; use Droopi.Setup.Test;
with Droopi.No_Tasking;
with Droopi.ORB.Task_Policies;

procedure Droopi.Test.No_Tasking is
begin
   Initialize_Test_Server
     (Droopi.No_Tasking.Initialize'Access,
      new Droopi.ORB.Task_Policies.No_Tasking);
   Initialize_Test_Access_Points;
   Initialize_Test_Object;
   Run_Test;
end Droopi.Test.No_Tasking;
