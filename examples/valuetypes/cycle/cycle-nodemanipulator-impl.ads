----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with cycle.Node;
with PortableServer;

package cycle.NodeManipulator.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   procedure remoteManipulate
     (Self : access Object;
      n : in out cycle.Node.Value_Ref);

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end cycle.NodeManipulator.Impl;
