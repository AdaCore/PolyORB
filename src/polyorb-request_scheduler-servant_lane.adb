------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.REQUEST_SCHEDULER.SERVANT_LANE                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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

with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.Exceptions;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Lanes;
with PolyORB.References.Binding;
with PolyORB.RT_POA_Policies.Thread_Pool_Policy;
with PolyORB.Servants;
with PolyORB.Setup;
with PolyORB.Task_Info;
with PolyORB.Utils.Strings;

package body PolyORB.Request_Scheduler.Servant_Lane is

   ---------------------------
   -- Try_Queue_Request_Job --
   ---------------------------

   function Try_Queue_Request_Job
     (Self   : access Request_Scheduler_Servant_Lane;
      Job    :        PolyORB.Jobs.Job_Access;
      Target :        PolyORB.References.Ref)
     return Boolean
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warnings (On);  --  WAG:3.15

      use PolyORB.Lanes;
      use PolyORB.Servants;

      use type Task_Info.Task_Info_Access;
      Surrogate : Components.Component_Access;
      Pro : PolyORB.Binding_Data.Profile_Access;
      Error : Exceptions.Error_Container;

   begin
      --  First test wether the target is a local servant
      --  managed by a RT-POA.

      References.Binding.Bind
        (Target,
         PolyORB.Setup.The_ORB,
         Surrogate, Pro, False, Error);
      --  XXX Should remove dependency on The_ORB

      if Surrogate.all in Servant'Class then
         declare
            use PolyORB.RT_POA_Policies.Thread_Pool_Policy;
            use PolyORB.Lanes;

            To_Lane : constant Lane_Root_Access
              := Get_Servant_Lane
              (PolyORB.Servants.Servant_Access (Surrogate));

         begin
            if To_Lane /= null then
               --  Queue request to the lane attached to servant

               Queue_Job (Lane_Access (To_Lane), Job);

               return True;
            end if;
         end;
      end if;

      return False;
   end Try_Queue_Request_Job;

   ------------
   -- Create --
   ------------

   function Create
     (RCF : access Request_Scheduler_Servant_Lane_Factory)
     return Request_Scheduler_Access
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (RCF);
      pragma Warnings (On);  --  WAG:3.15

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
       Init      => Initialize'Access));
end PolyORB.Request_Scheduler.Servant_Lane;
