--  Setup a test server with no tasking at all.

--  $Id$

with PolyORB.Setup.Test; use PolyORB.Setup.Test;
with PolyORB.Setup.Test_CORBA;
with PolyORB.No_Tasking;
with PolyORB.ORB.Task_Policies;

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);
with CORBA.Object;
pragma Warnings (Off, CORBA.Object);
with CORBA.AbstractBase;
pragma Warnings (Off, CORBA.AbstractBase);


procedure PolyORB.Test.No_Tasking_POA is
begin
   Initialize_Test_Server
     (PolyORB.No_Tasking.Initialize'Access,
      new PolyORB.ORB.Task_Policies.No_Tasking);
   Initialize_Test_Access_Points;
   Setup.Test_CORBA.Initialize_CORBA_Test_Object;
   Run_Test;
end PolyORB.Test.No_Tasking_POA;
