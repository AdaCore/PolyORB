--  Setup a test server with no tasking at all.

--  $Id$

with PolyORB.Setup.Test; use PolyORB.Setup.Test;
with PolyORB.No_Tasking;
with PolyORB.ORB.Task_Policies;

procedure PolyORB.Test.No_Tasking is
begin
   Initialize_Test_Server
     (PolyORB.No_Tasking.Initialize'Access,
      new PolyORB.ORB.Task_Policies.No_Tasking);
   Initialize_Test_Access_Points;
   Initialize_Test_Object;
   Run_Test;
end PolyORB.Test.No_Tasking;
