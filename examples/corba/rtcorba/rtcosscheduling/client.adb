------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Ada.Command_Line;
with Ada.Dynamic_Priorities;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.ORB;

with Echo;

with RTCORBA.Current.Helper;
with RTCORBA.PriorityMapping.Linear;

with RTCosScheduling.ClientScheduler.Impl;

with PolyORB.RTCORBA_P.Setup;
with PolyORB.Smart_Pointers;

--  Begin of PolyORB's setup

with PolyORB.ORB.Thread_Pool;
pragma Elaborate_All (PolyORB.ORB.Thread_Pool);
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

with PolyORB.Setup.Server;
pragma Elaborate_All (PolyORB.Setup.Server);
pragma Warnings (Off, PolyORB.Setup.Server);

with PolyORB.ORB_Controller.Half_Sync_Half_Async;
pragma Warnings (Off, PolyORB.ORB_Controller.Half_Sync_Half_Async);
pragma Elaborate_All (PolyORB.ORB_Controller.Half_Sync_Half_Async);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities;
pragma Elaborate_All
  (PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads;
pragma Elaborate_All (PolyORB.Tasking.Profiles.Full_Tasking.Threads);
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations;
pragma Elaborate_All
  (PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations);
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations);

with PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
pragma Elaborate_All (PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);

with PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
pragma Elaborate_All
  (PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);

with PolyORB.QoS.Priority;
pragma Elaborate_All (PolyORB.QoS.Priority);
pragma Warnings (Off, PolyORB.QoS.Priority);

--  End of PolyORB's setup

with PolyORB.Utils.Report;

procedure Client is
   use Ada.Command_Line;
   use Ada.Dynamic_Priorities;
   use Ada.Text_IO;

   use CORBA;
   use CORBA.ORB;
   use RTCORBA;

   use PolyORB.Utils.Report;

   Priority_Mapping : RTCORBA.PriorityMapping.Linear.Object;

   Sent_Msg : CORBA.String;
   Running_Priority : CORBA.Short;
   myecho : Echo.Ref;

   Native_Rounded_Priority : RTCORBA.NativePriority;
   Rounded_Priority : RTCORBA.Priority;
   Ok : Boolean;

begin
   CORBA.ORB.Initialize ("ORB");

   if Argument_Count /= 1 then
      Put_Line ("usage : client <IOR_string_from_server>");
      return;
   end if;

   New_Test ("RTCosScheduling client");

   --  Setting up default Priority Mapping for this node

   PolyORB.RTCORBA_P.Setup.Set_Priority_Mapping (Priority_Mapping);

   Output ("ORB is configured", True);

   --  Getting the CORBA Object

   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), myecho);

   --  Checking if it worked

   if Echo.Is_Nil (myecho) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  Test #0, invocation without setting client's priority

   --  Sending message

   Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
   Running_Priority := Echo.echoString (myecho, Sent_Msg);

   --  Printing result

   Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
   Put_Line ("Request executed at priority:"
             & CORBA.Short'Image (Running_Priority));

   --  Computing rounded priority

   RTCORBA.PriorityMapping.To_Native
     (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
      20_000,
      Native_Rounded_Priority,
      Ok);

   if not Ok then
      raise Program_Error;
   end if;

   RTCORBA.PriorityMapping.To_CORBA
     (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
      Native_Rounded_Priority,
      Rounded_Priority,
      Ok);

   if not Ok then
      raise Program_Error;
   end if;

   Output ("Running priority is correct",
           Running_Priority = CORBA.Short (Rounded_Priority));

   --  Test #1, invocation using ClientScheduler information

   declare
      Current : constant RTCORBA.Current.Local_Ref :=
                  RTCORBA.Current.Helper.To_Local_Ref
                    (Resolve_Initial_References
                     (To_CORBA_String ("RTCurrent")));

      Client_Scheduler_Object :
        constant RTCosScheduling.ClientScheduler.Impl.Object_Ptr
        := new RTCosScheduling.ClientScheduler.Impl.Object;

      Client_Scheduler : RTCosScheduling.ClientScheduler.Local_Ref;

   begin
      RTCosScheduling.ClientScheduler.Set
        (Client_Scheduler,
         PolyORB.Smart_Pointers.Entity_Ptr (Client_Scheduler_Object));
      Output ("ClientScheduler created", True);

      --  Reading configuration file

      RTCosScheduling.ClientScheduler.Impl.Load_Configuration_File
        ("client_scheduling.conf");
      Output ("Read configuration file", True);

      --  Set up activity1

      RTCosScheduling.ClientScheduler.schedule_activity
        (Client_Scheduler, CORBA.To_CORBA_String ("activity1"));

      Output ("Retrieve RTCurrent priority raised no exception",
              RTCORBA.Current.Get_The_Priority (Current) = 10_000);

      --  Sending message

      Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
      Running_Priority := Echo.echoString (myecho, Sent_Msg);

      --  Printing result

      Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
      Put_Line ("Request executed at priority:"
                & CORBA.Short'Image (Running_Priority));

      --  Test running Priority is compatible with our priority
      --  Implementation Note: this test relies on the fact that
      --  client and server nodes are homogeneous.

      --  Computing rounded priority

      RTCORBA.PriorityMapping.To_CORBA
        (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
         RTCORBA.NativePriority (Get_Priority),
         Rounded_Priority,
         Ok);

      if not Ok then
         raise Program_Error;
      end if;

      Output ("Running priority is correct",
              Running_Priority = CORBA.Short (Rounded_Priority));
   end;

   End_Report;
   CORBA.ORB.Shutdown (False);

exception
   when E : others =>
      Put_Line ("Got exception: " & Ada.Exceptions.Exception_Information (E));
      End_Report;

end Client;
