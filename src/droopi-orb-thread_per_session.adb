with Ada.Exceptions;

with Droopi.Log;
with Droopi.Jobs;
with Droopi.Components;
with Droopi.Filters;
with Droopi.Filters.Interface;
with Droopi.ORB.Interface;

with Droopi.Protocols;

with Locked_Queue;
pragma Elaborate_All (Locked_Queue);

pragma Elaborate_All (Droopi.Log);

package body Droopi.ORB.Thread_Per_Session is

   ------------------------
   -- Local declarations --
   ------------------------

   use Droopi.Asynch_Ev;
   use Droopi.Components;
   use Droopi.Filters;
   use Droopi.Filters.Interface;
   use Droopi.Log;
   use Droopi.Soft_Links;
   use Droopi.Annotations;
   use Droopi.Components;
   use Droopi.Transport;
   use Droopi.Protocols;
   use Droopi.ORB.Interface;

   package L is new Droopi.Log.Facility_Log
     ("droopi.orb.thread_per_session");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   task type Session_Thread is
      entry Start (A_W    : in Watcher_Access;
                   A_Msg : Message'Class;
                   A_S   : Session_Access;
                   A_ORB : ORB_Access);
   end Session_Thread;

   type Session_Thread_Access is access Session_Thread;

   type Request_Info is record
      Job : Jobs.Job_Access;
   end record;

   package Request_Queue is new Locked_Queue (Request_Info);

   type Queue_Access is access Request_Queue.Queue;

   type Queue_Indication is
     new Droopi.Filters.Interface.Data_Indication
     with record
        Queue : Queue_Access;
     end record;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      C   : Active_Connection)
   is
   begin
      pragma Debug (O (" new server connection. "));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));
   end Handle_New_Server_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      C   : Active_Connection)
   is
   begin
      pragma Debug (O (" new client connection"));
      Insert_Source (ORB, C.AES);
      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Confirmation'(null record));
   end Handle_New_Client_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   procedure Handle_Request_Execution
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      RJ  : Jobs.Job_Access)
   is
   begin
      Jobs.Run (RJ);
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P : access Thread_Per_Session_Policy;
      ORB : ORB_Access)
   is
   begin
      null;
   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      Msg : Message'Class)
   is
      Dummy_Task : Session_Thread_Access;
   begin
      if Msg in Interface.Queue_Request then
         declare
            QR : Interface.Queue_Request
              renames Interface.Queue_Request (Msg);
            S : Session_Access := Session_Access (QR.Requestor);
            W : Watcher_Access := Get_Request_Watcher (S);
         begin
            if W = null then
               --  Session request watcher is null : create task to
               --  handle this session.
               Dummy_Task := new Session_Thread;
               Dummy_Task.Start (W, Msg, S, ORB);

            else
               --  Session request watcher is not null, ie a task is
               --  already handling jobs from this session : emit the
               --  queue_request message.
               Emit_No_Reply
                 (Component_Access (S),
                  Queue_Request (Msg));
            end if;

         exception
            when E : others =>
               O ("Got exception while sending request_to_handler");
               O (Ada.Exceptions.Exception_Information (E));
         end;
      else
         raise Unhandled_Message;
      end if;
   end Queue_Request_To_Handler;

   --------------------
   -- Session_Thread --
   --------------------

   N : Natural := 0;
   --  For debugging purposes.

   task body Session_Thread
   is
      W   : Watcher_Access;
      V   : Version_Id;
      S   : Session_Access;
      Q   : Queue_Request;
      ORB : ORB_Access;
   begin
      accept Start (A_W   : in Watcher_Access;
                    A_Msg : Message'Class;
                    A_S   : Session_Access;
                    A_ORB : ORB_Access)
      do
         N := N + 1;
         pragma Debug (O ("Session Thread number "
                          & Integer'Image (N)
                          & " is starting"));

         ORB := A_ORB;
         W := A_W;
         S := A_S;
         Create (W);
         Set_Request_Watcher (A_S, W);
         Lookup (W, V);
         Emit_No_Reply (Component_Access (A_S),
                        Queue_Request (A_Msg));
      end Start;
      loop
         Differ (W, V);
         Lookup (W, V);
         Q := Get_Pending_Request (S);
         declare
            J : constant Jobs.Job_Access := new Request_Job;
            Req : Requests.Request_Access renames Q.Request;
         begin
            pragma Debug (O ("Queue_Request: enter"));
            Request_Job (J.all).ORB       := ORB;
            Request_Job (J.all).Request   := Req;

            if Q.Requestor = null then
               --  Should never reach this.
               Request_Job (J.all).Requestor
                 := Component_Access (ORB);
            else
               Request_Job (J.all).Requestor := Q.Requestor;
            end if;

            Jobs.Run (J);
         end;
      end loop;

   end Session_Thread;

end Droopi.ORB.Thread_Per_Session;
