--  Tasking policy for the ORB core: 'No_Tasking'.

--  $Id$

with Droopi.Components;
with Droopi.Filters.Interface;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

package body Droopi.ORB.Task_Policies is

   use Droopi.Components;
   use Droopi.Filters.Interface;
   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log
     ("droopi.orb.tasking_policies");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Handle_New_Server_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection) is
   begin
      pragma Debug (O ("No_Tasking: new server connection"));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));

      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.
   end Handle_New_Server_Connection;

   procedure Handle_New_Client_Connection
     (P   : access No_Tasking;
      ORB : ORB_Access;
      C   : Active_Connection) is
   begin
      pragma Debug (O ("No_Tasking: new client connection"));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Confirmation'(null record));

      --  The newly-created channel will be monitored
      --  by general-purpose ORB tasks.
   end Handle_New_Client_Connection;

   procedure Handle_Request_Execution
     (P   : access No_Tasking;
      ORB : ORB_Access;
      RJ  : Jobs.Job_Access) is
   begin
      pragma Debug (O ("No_Tasking: request execution"));

      Jobs.Run (RJ);
      --  No tasking: execute the request in the current task.
   end Handle_Request_Execution;

   procedure Idle (P : access No_Tasking; ORB : ORB_Access) is
   begin
      pragma Debug (O ("No_Tasking: Idle (BAD BAD!)"));
      raise Program_Error;
      --  When there is no tasking, the (only) task in the
      --  application may not go idle, since this would
      --  block the whole system forever.
   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access No_Tasking;
      ORB : ORB_Access;
      Msg : Message'Class)
   is
   begin
      Emit_No_Reply (Component_Access (ORB), Msg);
   end Queue_Request_To_Handler;

end Droopi.ORB.Task_Policies;
