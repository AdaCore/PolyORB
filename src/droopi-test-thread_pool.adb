--  Set up a test server with the Thread_Pool tasking policy.

--  $Id$

with Droopi.Setup.Test; use Droopi.Setup.Test;
with Droopi.Protected_Objects;
with Droopi.ORB.Thread_Pool;

procedure Droopi.Test.Thread_Pool is
begin
   Initialize_Test_Server
     (Droopi.Protected_Objects.Initialize'Access,
      new Droopi.ORB.Thread_Pool.Thread_Pool_Policy);
   ORB.Thread_Pool.Initialize (4, 10);

   Initialize_Test_Access_Points;
   Initialize_Test_Object;
   Run_Test;
end Droopi.Test.Thread_Pool;
