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
with PolyORB.Initialization;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Jobs;
with PolyORB.Log;
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
   use PolyORB.Components;
   use PolyORB.Transport;
   use PolyORB.Protocols;
   use PolyORB.ORB.Interface;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_per_session");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   --  For debugging purposes.

   A_W   : Watcher_Access := null;
   A_S   : Session_Access := null;
   A_ORB : ORB_Access     := null;
   --  This variables are used to initialized the threads local variables.
   --  They are used to replaced the accept statement

   Thread_Init_Watcher    : Watcher_Access := null;
   Thread_Init_Version_Id : Version_Id;
   --  This Watcher and his associated Version Id are used during
   --  the initialisation of a thread.

   -------------------------
   -- Main_Session_Thread --
   -------------------------

   --  This procedure is a parameterless procedure used as
   --  the body of the threads
   procedure Main_Session_Thread;

   procedure Main_Session_Thread
   is
      W   : Watcher_Access;
      V   : Version_Id;
      S   : Session_Access;
      Q   : Request_Info;
      ORB : ORB_Access;
   begin
         pragma Debug (O ("Session Thread number "
                          & Image (Current_Task)
                          & " is starting"));
         --  initialisation of local variables of the thread
         ORB := A_ORB;
         W := A_W;
         S := A_S;
         --  initialisation of the session's variables
         Create (W);
         Set_Request_Watcher (S, W);
         Lookup (W, V);
         --  release of the watcher
         Update (Thread_Init_Watcher);
         loop
            pragma Debug (O ("Thread number"
                             & Image (Current_Task)
                             & " is waiting"));
            Differ (W, V);
            if not Is_Open (S) then
               exit;
            end if;
            Lookup (W, V);
            Get_First_Request (S, Q);
            pragma Debug (O ("Thread number"
                             & Image (Current_Task)
                             & " is executing Job"));
            Run_Request (Request_Job (Q.Job.all)'Access);
            Jobs.Free (Q.Job);
            pragma Debug (O ("Thread number"
                             & Image (Current_Task)
                             & " has executed Job"));
         end loop;
         Can_Close_Session (S);
         pragma Debug (O ("Thread "
                          & Image (Current_Task)
                          & " stopped"));
   end Main_Session_Thread;


   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      C   : Active_Connection)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
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
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
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
      RJ  : access Request_Job'Class)
   is
      S : Session_Access := null;
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      pragma Debug (O ("Handle_Request_Execution : Run Job"));

      S := Session_Access (RJ.Requestor);
      Add_Request
        (S, Request_Info'(Job => PolyORB.ORB.Duplicate_Request_Job (RJ)));
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P : access Thread_Per_Session_Policy;
      ORB : ORB_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);
      null;
      --  XXX This is probably wrong!
      raise Program_Error;
      --  (XXX just in case).
   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      Msg : Message'Class)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);
      if Msg in Interface.Queue_Request then
         declare
            QR : Interface.Queue_Request
              renames Interface.Queue_Request (Msg);
            S : constant Session_Access := Session_Access (QR.Requestor);
            W : constant Watcher_Access := Get_Request_Watcher (S);
         begin
            if W = null then
               --  Session request watcher is null : create task to
               --  handle this session.
               A_W := W;
               A_S := S;
               A_ORB := ORB;
               Create_Task (Main_Session_Thread'Access);
               Differ (Thread_Init_Watcher, Thread_Init_Version_Id);
               Lookup (Thread_Init_Watcher, Thread_Init_Version_Id);
               --  wait until the end of thread initialisation before emiting
            end if;
               --  Session request watcher is not null, ie a task is
               --  already handling jobs from this session : emit the
               --  queue_request message.
               Emit_No_Reply
                 (Component_Access (ORB), Msg);
         exception
            when E : others =>
               O ("Got exception while sending request_to_handler");
               O (Ada.Exceptions.Exception_Information (E));
         end;
      else
         pragma Debug (O ("Queue Request To Handler"));
         raise Unhandled_Message;
      end if;
   end Queue_Request_To_Handler;

   ----------------
   -- Initialize --
   ----------------
   procedure Initialize;
   procedure Initialize is
   begin
      Setup.The_Tasking_Policy := new Thread_Per_Session_Policy;
      Create (Thread_Init_Watcher);
      Lookup (Thread_Init_Watcher, Thread_Init_Version_Id);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
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
