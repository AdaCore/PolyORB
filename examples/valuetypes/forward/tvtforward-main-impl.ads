----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with TVTForward.second;
with TVTForward.first;
with PortableServer;

package TVTForward.Main.Impl is

   type Object is
     new PortableServer.Servant_Base with private;

   type Object_Ptr is access all Object'Class;

   function sendSecond
     (Self : access Object;
      f : in TVTForward.first.Value_Ref)
     return TVTForward.second.Value_Ref;

private

   type Object is
     new PortableServer.Servant_Base with record
      --  Insert components to hold the state
      --  of the implementation object.
      null;
   end record;

end TVTForward.Main.Impl;
