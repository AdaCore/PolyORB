--  Tasking policy for the ORB core: 'No_Tasking'.

--  $Id$

with PolyORB.Components;
with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.ORB.Task_Policies is

   use PolyORB.Components;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.tasking_policies");
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
      RJ  : access Jobs.Job'Class) is
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

end PolyORB.ORB.Task_Policies;
