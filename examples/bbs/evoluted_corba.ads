with DSA_Server; use DSA_Server;
pragma Elaborate_All (DSA_Server);

package Evoluted_CORBA is

   My_Server   : DSA_Server.Ref;
   --  The server.

   type String_Ptr is access String;
   Penpal_Name : String_Ptr;
   --  Penpal : aliased Penpal_Type;
   --  The penpal representing the user

   procedure Mainloop;
   --  Enter the mainloop

end Evoluted_CORBA;
