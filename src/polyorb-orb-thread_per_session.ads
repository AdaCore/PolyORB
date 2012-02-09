------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ S E S S I O N        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

with PolyORB.Annotations;
with PolyORB.Jobs;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Tasking.Condition_Variables;
with PolyORB.Tasking.Mutexes;

package PolyORB.ORB.Thread_Per_Session is

   pragma Elaborate_Body;

   use PolyORB.Components;
   use PolyORB.Jobs;
   use PolyORB.Transport;

   -----------------------------------------------------------
   -- Implementation of a thread-per-session tasking policy --
   -----------------------------------------------------------

   type Thread_Per_Session_Policy is new Tasking_Policy_Type with private;

   type End_Thread_Job is new Jobs.Job with null record;
   --  This particular job is used to indicate to a thread associated with a
   --  session that it has to exit its main loop.

   type End_Thread_Job_Access is access all End_Thread_Job;

   overriding procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection);

   overriding procedure Handle_Close_Connection
     (P   : access Thread_Per_Session_Policy;
      TE  :        Transport_Endpoint_Access);

   overriding procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection);

   overriding procedure Handle_Request_Execution
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class);

   overriding procedure Idle
     (P         : access Thread_Per_Session_Policy;
      This_Task : PTI.Task_Info_Access;
      ORB       : ORB_Access);

   overriding procedure Run (J : not null access End_Thread_Job);

private

   type Request_Info is record
      Job : Jobs.Job_Access;
   end record;
   --  Request_Info is the type of the elements stored in the threads queues

   package Request_Queues is new PolyORB.Utils.Chained_Lists (Request_Info);

   subtype Request_Queue is Request_Queues.List;

   type Request_Queue_Access is access all Request_Queue;
   --  Request queue attached to a thread

   type Session_Thread_Info is new PolyORB.Annotations.Note with record
      Request_List : Request_Queue_Access;
      Request_M    : Tasking.Mutexes.Mutex_Access;
      Request_CV   : Tasking.Condition_Variables.Condition_Access;
   end record;
   --  Management of a session's request queue, protected by its mutex and cv

   procedure Add_Request
     (S  : Session_Thread_Info;
      RI : Request_Info);
   --  Add a job to a job queue

   type Thread_Per_Session_Policy is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Thread_Per_Session;
