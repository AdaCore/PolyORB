--  Helper functions for CORBA servers.

--  $Id$

with CORBA.Object;
with PortableServer;

package Droopi.CORBA_P.Server_Tools is

   pragma Elaborate_Body;

   procedure Initiate_Server (Start_New_Task : Boolean := True);
   --  This procedure starts a new ORB. If Start_New_Task is True,
   --  a new task will be created and control will be returned to the
   --  caller. Otherwise, the ORB will be executing in the current
   --  context.

   procedure Initiate_Servant
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);

   procedure Reference_To_Servant
     (R : in CORBA.Object.Ref'Class;
      S : out PortableServer.Servant);

   procedure Servant_To_Reference
     (S : in PortableServer.Servant;
      R : out CORBA.Object.Ref'Class);

end Droopi.CORBA_P.Server_Tools;
