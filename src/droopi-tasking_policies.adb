--  $Id$

package body Droopi.Tasking_Policies is

   procedure Handle_New_Connection
     (P : access No_Tasking; C : Channel) is
   begin

      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.

      Insert_Channel (ORB, C);

   end;

   procedure Handle_Request
     (P : access No_Tasking; R : Request) is
   begin
      J := Create_Job_For_Request (R);
      Schedule_Job (J);
   end;

end Droopi.Tasking_Policies;
