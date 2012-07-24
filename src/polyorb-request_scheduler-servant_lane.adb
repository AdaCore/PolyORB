------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.REQUEST_SCHEDULER.SERVANT_LANE                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Errors;
with PolyORB.Initialization;

with PolyORB.Lanes;
with PolyORB.Log;
with PolyORB.References.Binding;
with PolyORB.RT_POA_Policies.Priority_Model_Policy;
with PolyORB.RT_POA_Policies.Thread_Pool_Policy;
with PolyORB.Servants;
with PolyORB.Setup;
with PolyORB.Task_Info;
with PolyORB.Tasking.Priorities;
with PolyORB.Utils.Strings;

package body PolyORB.Request_Scheduler.Servant_Lane is
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.request_scheduler.servant_lane");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   ---------------------------
   -- Try_Queue_Request_Job --
   ---------------------------

   overriding function Try_Queue_Request_Job
     (Self   : access Request_Scheduler_Servant_Lane;
      Job    : PolyORB.Jobs.Job_Access;
      Target : PolyORB.References.Ref) return Boolean
   is
      pragma Unreferenced (Self);

      use PolyORB.Errors;
      use PolyORB.Lanes;
      use PolyORB.Servants;

      use type Task_Info.Task_Info_Access;
      Surrogate : Components.Component_Access;
      Pro : PolyORB.Binding_Data.Profile_Access;
      Error : Errors.Error_Container;

   begin
      pragma Debug (C, O ("Try_Queue_Request_Job: enter"));

      --  First test whether the target is a local servant managed by a RT-POA.

      References.Binding.Bind
        (Target,
         PolyORB.Setup.The_ORB, (others => null),
         Surrogate, Pro,
         Local_Only => False,
         Error      => Error);
      --  XXX Should remove dependency on The_ORB
      --  We're only interested in determining whether we have a local RT
      --  servant, why don't we bind with Local_Only => True???

      if Found (Error) then
         Catch (Error);
         return False;
      end if;

      if Surrogate.all in Servant'Class then
         declare
            use PolyORB.RT_POA_Policies.Priority_Model_Policy;
            use PolyORB.RT_POA_Policies.Thread_Pool_Policy;

            To_Lane : constant Lane_Root_Access :=
              Get_Servant_Lane (PolyORB.Servants.Servant_Access (Surrogate));

         begin
            if To_Lane /= null then
               --  Queue request to the lane attached to servant

               declare
                  use PolyORB.Tasking.Priorities;

                  Model : Priority_Model;
                  Server_ORB_Priority : ORB_Priority;
                  Server_External_Priority : External_Priority;
                  Error : PolyORB.Errors.Error_Container;

               begin
                  Get_Servant_Priority_Information
                    (PolyORB.Servants.Servant_Access (Surrogate),
                     Model,
                     Server_ORB_Priority,
                     Server_External_Priority,
                     Error);

                  if Found (Error) then
                     Catch (Error);
                     pragma Debug (C, O ("No priority information"));
                     pragma Debug (C, O ("Try_Queue_Request_Job: leave"));

                     return False;
                  end if;

                  Queue_Job (To_Lane, Job, Server_External_Priority);
                  pragma Debug (C, O ("Job queued"));
                  pragma Debug (C, O ("Try_Queue_Request_Job: leave"));
                  return True;
               end;
            end if;
         end;
      end if;

      pragma Debug (C, O ("No lane attached to servant, cannot queue job"));
      pragma Debug (C, O ("Try_Queue_Request_Job: leave"));
      return False;
   end Try_Queue_Request_Job;

   ------------
   -- Create --
   ------------

   overriding function Create
     (RCF : access Request_Scheduler_Servant_Lane_Factory)
     return Request_Scheduler_Access
   is
      pragma Unreferenced (RCF);

   begin
      return new Request_Scheduler_Servant_Lane;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_Request_Scheduler_Factory (RCF);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"request_scheduler.servant_lane",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"request_scheduler",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Request_Scheduler.Servant_Lane;
