--  $Id$

package body Droopi.Tasking_Policies is

   use Droopi.Sockets;

   procedure Handle_New_Connection
     (P : access No_Tasking; S : Socket_Type) is
   begin

      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.

      Insert_Socket (ORB, S, Listening_Sk);

   end;

   procedure Handle_Request
     (P : access No_Tasking; R : Droopi.Requests.Request) is
   begin
      J := Create_Job_For_Request (R);
      Schedule_Job (J);
   end;

end Droopi.Tasking_Policies;
