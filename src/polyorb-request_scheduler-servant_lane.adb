------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.REQUEST_SCHEDULER.SERVANT_LANE                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2006, Free Software Foundation, Inc.          --
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
   pragma Unreferenced (C); --  For conditional pragma Debug

   ---------------------------
   -- Try_Queue_Request_Job --
   ---------------------------

   function Try_Queue_Request_Job
     (Self   : access Request_Scheduler_Servant_Lane;
      Job    :        PolyORB.Jobs.Job_Access;
      Target :        PolyORB.References.Ref)
     return Boolean
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
      pragma Debug (O ("Try_Queue_Request_Job: enter"));

      --  First test wether the target is a local servant
      --  managed by a RT-POA.

      References.Binding.Bind
        (Target,
         PolyORB.Setup.The_ORB, (others => null),
         Surrogate, Pro, False, Error);
      --  XXX Should remove dependency on The_ORB

      if Found (Error) then
         Catch (Error);
         return False;
      end if;

      if Surrogate.all in Servant'Class then
         declare
            use PolyORB.RT_POA_Policies.Priority_Model_Policy;
            use PolyORB.RT_POA_Policies.Thread_Pool_Policy;

            To_Lane : constant Lane_Root_Access
              := Get_Servant_Lane
              (PolyORB.Servants.Servant_Access (Surrogate));

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
                     pragma Debug (O ("No priority information"));
                     pragma Debug (O ("Try_Queue_Request_Job: leave"));

                     return False;
                  end if;

                  Queue_Job (To_Lane, Job, Server_External_Priority);
                  pragma Debug (O ("Job queued"));
                  pragma Debug (O ("Try_Queue_Request_Job: leave"));
                  return True;
               end;
            end if;
         end;
      end if;

      pragma Debug (O ("No lane attached to servant, cannot queue job"));
      pragma Debug (O ("Try_Queue_Request_Job: leave"));
      return False;
   end Try_Queue_Request_Job;

   ------------
   -- Create --
   ------------

   function Create
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
