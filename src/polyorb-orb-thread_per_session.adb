------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ S E S S I O N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Exceptions;

with PolyORB.Components;
with PolyORB.Configurator;
pragma Elaborate_All (PolyORB.Configurator);
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Jobs;
with PolyORB.Locked_Queue;
pragma Elaborate_All (PolyORB.Locked_Queue);
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.ORB.Interface;
with PolyORB.Protocols;
with PolyORB.Setup;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Per_Session is

   ------------------------
   -- Local declarations --
   ------------------------

   use PolyORB.Asynch_Ev;
   use PolyORB.Components;
   use PolyORB.Filters;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.Soft_Links;
   use PolyORB.Annotations;
   use PolyORB.Components;
   use PolyORB.Transport;
   use PolyORB.Protocols;
   use PolyORB.ORB.Interface;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_per_session");
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
     new PolyORB.Filters.Interface.Data_Indication
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
      RJ  : access Jobs.Job'Class)
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

   procedure Initialize;
   procedure Initialize is
   begin
      Setup.The_Tasking_Policy := new Thread_Per_Session_Policy;
   end Initialize;

   use PolyORB.Configurator;
   use PolyORB.Configurator.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name => +"orb.thread_per_session",
       Conflicts => +"no_tasking",
       Depends => +"soft_links",
       Provides => +"orb.tasking_policy",
       Init => Initialize'Access));
end PolyORB.ORB.Thread_Per_Session;
