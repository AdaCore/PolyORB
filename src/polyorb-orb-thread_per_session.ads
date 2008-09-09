------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--       P O L Y O R B . O R B . T H R E A D _ P E R _ S E S S I O N        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Annotations;
with PolyORB.Jobs;
with PolyORB.Utils.Chained_Lists;
with PolyORB.Tasking.Semaphores;

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

   procedure Handle_New_Server_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection);

   procedure Handle_Close_Connection
     (P   : access Thread_Per_Session_Policy;
      TE  :        Transport_Endpoint_Access);

   procedure Handle_New_Client_Connection
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      AC  :        Active_Connection);

   procedure Handle_Request_Execution
     (P   : access Thread_Per_Session_Policy;
      ORB :        ORB_Access;
      RJ  : access Request_Job'Class);

   procedure Idle
     (P         : access Thread_Per_Session_Policy;
      This_Task : in out PolyORB.Task_Info.Task_Info;
      ORB       :        ORB_Access);

   procedure Run (J : not null access End_Thread_Job);

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
      Request_Semaphore : Tasking.Semaphores.Semaphore_Access := null;
      Request_List      : Request_Queue_Access := null;
   end record;
   --  This structure is used in order to be able to retrieve the queue
   --  and the semaphore associated with a thread, with the knownledge of
   --  of a session access

   procedure Add_Request
     (S  : Session_Thread_Info;
      RI :    Request_Info);
   --  Add a job to a job queue

   type Thread_Per_Session_Policy is new Tasking_Policy_Type with null record;

end PolyORB.ORB.Thread_Per_Session;
