--  $Id$

--  XXX This must go away!
with Droopi.Sockets;

package body Droopi.ORB.Task_Policies is

   use Droopi.Filters;
   use Droopi.Filters.Data_Units;
   use Droopi.Sockets;

   procedure Handle_New_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      AS  : Active_Socket)
   is
      Dummy : Boolean;
   begin
      Insert_Socket (ORB, AS);
      Dummy := Filters.Handle_Message
        (AS.Channel, Connect_Indication'(null record));
      pragma Assert (Dummy);
      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.
   end Handle_New_Connection;

   procedure Handle_Request
     (P   : access No_Tasking;
      ORB : ORB_Access;
      R   : Droopi.Requests.Request) is
   begin
      --  J := Create_Job_For_Request (R);
      --  Schedule_Job (J);
      raise Not_Implemented;
   end Handle_Request;

end Droopi.ORB.Task_Policies;
