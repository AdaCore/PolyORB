with CORBA;

package Broca.Flags is
   --  The body is in charge to decode options during elaboration.
   pragma Elaborate_Body (Broca.Flags);

   --  Not highly used.
   Verbose : Boolean := True;

   --  If TRUE, server logs incoming requests, processing...
   --  Can be enable by -ORBlog
   Log : Boolean := False;

   --  TCP/IP port used.
   --  -ORBport N
   Port : Natural := 0;

   --  Number of tasks used by the server.
   --  If 1, it use the main task (ie, the task that has called run) to do all
   --  the work.  If greather than 1, it creates tasks.
   Nbr_Server_Tasks : Positive := 4;

   --  Maximum number of consecutive location forward.
   Max_Tries : Natural := 2;

   --  A coded value of time when the server was started up.  This is used in
   --  ObjectId.
   Boot_Time : CORBA.Unsigned_Long;
end Broca.Flags;
