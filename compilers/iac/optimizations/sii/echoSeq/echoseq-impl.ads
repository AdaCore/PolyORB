pragma Style_Checks (Off);

with PortableServer;

package echoSeq.Impl is

   --  This is simply used to define the operations.

   type Object is new PortableServer.Servant_Base with record
      null;
   end record;

   function echoUsequence
     (Self : access Object;
      arg : in U_sequence) return U_sequence;

end echoSeq.Impl;
