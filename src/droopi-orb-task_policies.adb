--  $Id$

with Droopi.Components;
with Droopi.Filters;

package body Droopi.ORB.Task_Policies is

   use Droopi.Components;
   use Droopi.Filters.Data_Units;

   procedure Handle_New_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection) is
   begin
      Insert_Source (ORB, C.AES);
      Components.Emit
        (Component_Access (C.TE), Connect_Indication'(null record));
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
