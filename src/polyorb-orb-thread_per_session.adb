------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ S E S S I O N        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Components;
with PolyORB.Filters;
with PolyORB.Filters.Iface;
with PolyORB.Initialization;

with PolyORB.Log;
with PolyORB.Protocols;
with PolyORB.Setup;
with PolyORB.Tasking.Threads;
with PolyORB.Utils.Strings;

package body PolyORB.ORB.Thread_Per_Session is

   use PolyORB.Annotations;
   use PolyORB.Filters;
   use PolyORB.Filters.Iface;
   use PolyORB.Log;
   use PolyORB.Protocols;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Tasking.Condition_Variables;
   use PolyORB.Tasking.Threads;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.orb.thread_per_session");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   type Session_Runnable is new Runnable with record
      ORB : ORB_Access;
      A_S : Session_Access;
   end record;

   overriding procedure Run (R : not null access Session_Runnable);

   procedure Initialize;

   ----------
   -- Free --
   ----------

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Notepad,
      Name => Notepad_Access);

   procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
     (Object => Request_Queue,
      Name => Request_Queue_Access);

   -----------------
   -- Add_Request --
   -----------------

   procedure Add_Request
     (S  : Session_Thread_Info;
      RI :    Request_Info) is
   begin
      Enter (S.Request_M);
      Request_Queues.Append (S.Request_List.all, RI);
      Leave (S.Request_M);
      Signal (S.Request_CV);

      pragma Debug (C, O ("A request has been queued"));
   end Add_Request;

   -----------------------------
   -- Handle_Close_Connection --
   -----------------------------

   overriding procedure Handle_Close_Connection
     (P   : access Thread_Per_Session_Policy;
      TE  :        Transport_Endpoint_Access)
   is
      pragma Unreferenced (P);

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

      --  Create and queue an End_Thread_Job

      declare
         ET : constant End_Thread_Job_Access := new End_Thread_Job;
         N  : constant Notepad_Access := Get_Task_Info (Session_Access (S));

         STI : Session_Thread_Info;
      begin
         Get_Note (N.all, STI);
         Add_Request
           (STI,
            Request_Info'(Job => Jobs.Job_Access (ET)));
      end;

      pragma Debug (C, O ("A End_Thread_Job has been queued"));
   end Handle_Close_Connection;

   ----------------------------------
   -- Handle_New_Client_Connection --
   ----------------------------------

   overriding procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection)
   is
      pragma Unreferenced (P, ORB);
   begin
      pragma Debug (C, O ("New client connection"));

      Components.Emit_No_Reply (Component_Access (AC.TE),
         Connect_Confirmation'(null record));
   end Handle_New_Client_Connection;

   ----------------------------------
   -- Handle_New_Server_Connection --
   ----------------------------------

   overriding procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection)
   is
      pragma Unreferenced (P);

      S    : Filters.Filter_Access;
      Temp : Filters.Filter_Access := Filters.Filter_Access (Upper (AC.TE));

      T    : Thread_Access;
      pragma Unreferenced (T);
      --  T is assigned but never read

   begin
      pragma Debug (C, O ("New server connection."));

      --  Determine ORB session attached to this connection

      while Temp /= null loop
         S := Temp;
         Temp := Filters.Filter_Access (Upper (Temp));
      end loop;
      pragma Assert (S /= null);

      --  Start session task

      T := Run_In_Task (Get_Thread_Factory,
                        R    => new Session_Runnable'(ORB => ORB,
                                                      A_S =>
                                                        Session_Access (S)),
                        Name => "Session");

      Components.Emit_No_Reply (Component_Access (AC.TE),
         Connect_Indication'(null record));
   end Handle_New_Server_Connection;

   ------------------------------
   -- Handle_Request_Execution --
   ------------------------------

   overriding procedure Handle_Request_Execution
     (P   : access Thread_Per_Session_Policy;
      ORB : ORB_Access;
      RJ  : access Request_Job'Class)
   is
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);

      S   : constant Session_Access :=
        Session_Access (RJ.Request.Requesting_Component);
      N   : constant Notepad_Access := Get_Task_Info (S);
      STI : Session_Thread_Info;
   begin
      --  Pass on request to session task

      Get_Note (N.all, STI);
      Add_Request (STI, Request_Info'(Job => Job_Access (RJ)));
   end Handle_Request_Execution;

   ----------
   -- Idle --
   ----------

   overriding procedure Idle
     (P         : access Thread_Per_Session_Policy;
      This_Task : PTI.Task_Info_Access;
      ORB       : ORB_Access)
   is
      pragma Unreferenced (P);
      pragma Unreferenced (ORB);

      package PTI  renames PolyORB.Task_Info;
   begin
      --  In Thread_Per_Session policy, only one task is executing ORB.Run.
      --  However, it can be set to idle while another thread modifies
      --  ORB internals.

      pragma Debug (C, O ("Thread "
                       & Image (PTI.Id (This_Task.all))
                       & " is going idle."));

      Wait (PTI.Condition (This_Task.all), PTI.Mutex (This_Task.all));

      pragma Debug (C, O ("Thread "
                       & Image (PTI.Id (This_Task.all))
                       & " is leaving Idle state"));
   end Idle;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Setup.The_Tasking_Policy := new Thread_Per_Session_Policy;
   end Initialize;

   ---------
   -- Run --
   ---------

   overriding procedure Run (J : not null access End_Thread_Job) is
      pragma Unreferenced (J);
   begin
      null;
   end Run;

   ---------
   -- Run --
   ---------

   overriding procedure Run (R : not null access Session_Runnable) is
      M   : Mutex_Access;
      CV  : Condition_Access;
      L   : Request_Queue_Access;
      N   : Notepad_Access;
      Q   : Request_Info;
   begin
      pragma Debug (C, O ("Session Thread number "
                       & Image (Current_Task)
                       & " is starting"));

      --  Runnable initialization

      Create (M);
      Create (CV);
      L := new Request_Queue;
      N := new Notepad;
      Set_Note (N.all,
                Session_Thread_Info'(Note with
                                       Request_M    => M,
                                       Request_CV   => CV,
                                       Request_List => L));
      Set_Task_Info (R.A_S, N);

      --  Runnable main loop

      loop
         pragma Debug (C, O ("Thread number"
                          & Image (Current_Task)
                          & " is waiting"));

         --  Fetch one element from L

         Enter (M);
         loop
            exit when Request_Queues.Length (L.all) > 0;
            Wait (CV, M);
         end loop;
         Request_Queues.Extract_First (L.all, Q);
         Leave (M);

         pragma Debug (C, O ("Thread number"
                          & Image (Current_Task)
                          & " is executing Job"));

         if Q.Job.all in Request_Job'Class then
            Run_Request (R.ORB, Request_Job (Q.Job.all).Request);
            Jobs.Free (Q.Job);
         elsif Q.Job.all in End_Thread_Job'Class then
            pragma Debug (C, O ("Received an End_Thread_Message"));
            Jobs.Free (Q.Job);
            exit;
         end if;

         pragma Debug (C, O ("Thread number"
                          & Image (Current_Task)
                          & " has executed Job"));
      end loop;

      --  Runnable finalization

      pragma Debug (C, O ("Finalizing thread " & Image (Current_Task)));
      Request_Queues.Deallocate (L.all);
      Free (L);
      Destroy (M);
      Destroy (CV);
      Destroy (N.all);
      Free (N);

      pragma Debug (C, O ("Thread "
                       & Image (Current_Task)
                       & " stopped"));
   end Run;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"orb.thread_per_session",
       Conflicts => +"no_tasking",
       Depends   => +"tasking.condition_variables",
       Provides  => +"orb.tasking_policy!",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.ORB.Thread_Per_Session;
