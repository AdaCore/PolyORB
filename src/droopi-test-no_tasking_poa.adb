--  Setup a test server with no tasking at all.

--  $Id$

with Droopi.Setup.Test; use Droopi.Setup.Test;
with Droopi.Setup.Test_CORBA;
with Droopi.No_Tasking;
with Droopi.ORB.Task_Policies;

with CORBA.Impl;
pragma Warnings (Off, CORBA.Impl);
with CORBA.Object;
pragma Warnings (Off, CORBA.Object);
with CORBA.AbstractBase;
pragma Warnings (Off, CORBA.AbstractBase);


procedure Droopi.Test.No_Tasking_POA is
begin
   Initialize_Test_Server
     (Droopi.No_Tasking.Initialize'Access,
      new Droopi.ORB.Task_Policies.No_Tasking);
   Initialize_Test_Access_Points;
   Setup.Test_CORBA.Initialize_CORBA_Test_Object;
   Run_Test;
end Droopi.Test.No_Tasking_POA;
