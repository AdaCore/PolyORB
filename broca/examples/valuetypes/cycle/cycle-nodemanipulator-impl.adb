----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with cycle.Node;
with cycle.NodeManipulator.Skel;

package body cycle.NodeManipulator.Impl is


   procedure remoteManipulate
     (Self : access Object;
      n : in out cycle.Node.Value_Ref) is
   begin
      N := Cycle.Node.CmdLineManipulate (N);
   end remoteManipulate;

end cycle.NodeManipulator.Impl;
