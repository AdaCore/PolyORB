--  Set up a test server with the Thread_Pool tasking policy.

--  $Id$

with PolyORB.Setup.Test; use PolyORB.Setup.Test;
with PolyORB.Protected_Objects;
with PolyORB.ORB.Thread_Pool;
with PolyORB.Setup.Test_CORBA;

procedure PolyORB.Test.Thread_Pool_POA is
begin
   Initialize_Test_Server
     (PolyORB.Protected_Objects.Initialize'Access,
      new PolyORB.ORB.Thread_Pool.Thread_Pool_Policy);
   ORB.Thread_Pool.Initialize (4, 10);

   Initialize_Test_Access_Points;
   Setup.Test_CORBA.Initialize_CORBA_Test_Object;
   Run_Test;
end PolyORB.Test.Thread_Pool_POA;
