------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ S E S S I O N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with Ada.Unchecked_Deallocation;

with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Filters.Interface;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Log;
with PolyORB.ORB.Interface;
with PolyORB.Protocols;
with PolyORB.Setup;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Per_Session is

   use PolyORB.Annotations;
   use PolyORB.Asynch_Ev;
   use PolyORB.Components;
   use PolyORB.Filters;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;
   use PolyORB.ORB.Interface;
   use PolyORB.Protocols;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Semaphores;
   use PolyORB.Tasking.Threads;
   use PolyORB.Transport;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_per_session");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   A_S   : Session_Access := null;
   --  This variable is used to initialize the threads local variable.
   --  it is used to replace the 'accept' statement.

   Session_Mutex : Mutex_Access;
   Session_Taken : Condition_Access;
   --  Synchornisation of task initialization.

   procedure Session_Thread;
   --  Main loop executed by session threads.

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (Notepad, Notepad_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Request_Queue, Request_Queue_Access);

   -----------------
   -- Add_Request --
   -----------------

   procedure Add_Request
     (S  : in Session_Thread_Info;
      RI :    Request_Info) is
   begin
      Request_Queues.Append (S.Request_List.all, RI);
      V (S.Request_Semaphore);

      pragma Debug (O ("A request has been queued"));
   end Add_Request;

   ------------------------------------
   -- Handle_Close_Server_Connection --
   ------------------------------------

   procedure Handle_Close_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      TE  :        Transport_Endpoint_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);

      use PolyORB.Components;

      ET : End_Thread_Job_Access;
      S  : Filters.Filter_Access := null;
   begin
      --  Find an access to the session
      declare
         Temp : Filters.Filter_Access := Filters.Filter_Access (Upper (TE));
      begin
         while Temp /= null loop
            S := Temp;
            Temp := Filters.Filter_Access (Upper (Temp));
         end loop;
      end;

      --  Create and queue a 'End_Thread_Job'
      ET := new End_Thread_Job;
      declare
         N   : constant Notepad_Access := Get_Task_Info (Session_Access (S));
         STI : Session_Thread_Info;
      begin
         Get_Note (N.all, STI);
         Add_Request
           (STI,
            Request_Info'(Job => Jobs.Job_Access (ET)));
      end;

      pragma Debug (O ("A End_Thread_Job has been queued"));
   end Handle_Close_Server_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      C   :        Active_Connection)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P, ORB);
      pragma Warnings (On);

   begin
      pragma Debug (O ("New client connection"));

      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Confirmation'(null record));
   end Handle_New_Client_Connection;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      C   :        Active_Connection)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P, ORB);
      pragma Warnings (On);

      S    : Filters.Filter_Access := null;
      Temp : Filters.Filter_Access := Filters.Filter_Access (Upper (C.TE));
   begin
      pragma Debug (O ("New server connection."));

      --  Determine ORB session attached to this connection.
      while Temp /= null loop
         S := Temp;
         Temp := Filters.Filter_Access (Upper (Temp));
      end loop;

      pragma Debug (O ("Found Session access"));

      if S = null then
         null;
         pragma Debug (O ("Session access isn't defined yet .."));
      end if;

      Enter (Session_Mutex);
      A_S := Session_Access (S);
      Create_Task (Session_Thread'Access);
      Wait (Session_Taken, Session_Mutex);
      Leave (Session_Mutex);

      Components.Emit_No_Reply
        (Component_Access (C.TE),
         Connect_Indication'(null record));
   end Handle_New_Server_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   procedure Handle_Request_Execution
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

      S : constant Session_Access := Session_Access (RJ.Requestor);
      N : constant Notepad_Access := Get_Task_Info (S);
      STI : Session_Thread_Info;
   begin
      pragma Debug (O ("Handle_Request_Execution : Queue Job"));

      Get_Note (N.all, STI);
      Add_Request
        (STI,
         Request_Info'(Job => PolyORB.ORB.Duplicate_Request_Job (RJ)));

   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (P         : access Thread_Per_Session_Policy;
      This_Task :        PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Unreferenced (This_Task);
      pragma Unreferenced (ORB);
      pragma Warnings (On);

   begin

      raise Program_Error;

      --  In Thread_Per_Session policy, only one task is executing
      --  ORB.Run. Thus blocking the task is an erroneous execution.

   end Idle;

   ------------------------------
   -- Queue_Request_To_Handler --
   ------------------------------

   procedure Queue_Request_To_Handler
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      Msg :        Message'Class)
   is
      pragma Warnings (Off);
      pragma Unreferenced (P);
      pragma Warnings (On);

   begin
      if Msg in Interface.Queue_Request then
         Emit_No_Reply
           (Component_Access (ORB), Msg);
      else
         pragma Debug (O ("Queue Request To Handler"));
         raise Unhandled_Message;
      end if;
   end Queue_Request_To_Handler;

   ---------
   -- Run --
   ---------

   procedure Run (J : access End_Thread_Job)
   is
      pragma Warnings (Off);
      pragma Unreferenced (J);
      pragma Warnings (On);

   begin
      null;
   end Run;

   --------------------
   -- Session_Thread --
   --------------------

   procedure Session_Thread
   is
      Sem : Semaphore_Access     := null;
      S   : Session_Access       := null;
      L   : Request_Queue_Access := null;
      N   : Notepad_Access       := null;
      Q   : Request_Info;
   begin
      pragma Debug (O ("Session Thread number "
                       & Image (Current_Task)
                       & " is starting"));
      --  Thread initialization.
      S := A_S;
      Create (Sem);
      L := new Request_Queue;
      N := new Notepad;
      Set_Note (N.all,
                Session_Thread_Info'(Note with Request_Semaphore => Sem,
                                     Request_List => L));
      Set_Task_Info (S, N);

      --  Signal end of thread initialization.
      Enter (Session_Mutex);
      Signal (Session_Taken);
      Leave (Session_Mutex);

      --  Session thread main loop.
      loop
         pragma Debug (O ("Thread number"
                          & Image (Current_Task)
                          & " is waiting"));

         P (Sem);
         Request_Queues.Extract_First (L.all, Q);
         pragma Debug (O ("Thread number"
                          & Image (Current_Task)
                          & " is executing Job"));

         if Q.Job.all in Request_Job'Class then
            Run_Request (Request_Job (Q.Job.all)'Access);
            Jobs.Free (Q.Job);
         elsif Q.Job.all in End_Thread_Job'Class then
            pragma Debug (O ("Received an End_Thread_Message"));
            Jobs.Free (Q.Job);
            exit;
         end if;

         pragma Debug (O ("Thread number"
                          & Image (Current_Task)
                          & " has executed Job"));
      end loop;

      --  Thread finalization.
      pragma Debug (O ("Finalizing thread " & Image (Current_Task)));
      Request_Queues.Deallocate (L.all);
      Free (L);
      Destroy (Sem);
      Destroy (N.all);
      Free (N);

      pragma Debug (O ("Thread "
                       & Image (Current_Task)
                       & " stopped"));
   end Session_Thread;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Setup.The_Tasking_Policy := new Thread_Per_Session_Policy;
      Create (Session_Mutex);
      Create (Session_Taken);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb.thread_per_session",
       Conflicts => +"no_tasking",
       Depends   => Empty,
       Provides  => +"orb.tasking_policy",
       Init      => Initialize'Access));
end PolyORB.ORB.Thread_Per_Session;
