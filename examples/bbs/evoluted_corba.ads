with DSA_Common.Penpal_Type.Impl;
with DSA_Server;

package Evoluted_CORBA is

   My_Server   : DSA_Server.Ref;
   --  The server.

   Penpal : aliased DSA_Common.Penpal_Type.Impl.Object;
   --  The penpal representing the user

   procedure Mainloop;
   --  Enter the mainloop

end Evoluted_CORBA;
