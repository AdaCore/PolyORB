------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

with PolyORB.RTCORBA_P.Setup;

--  Begin of PolyORB's setup

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

with PolyORB.Setup.Server;
pragma Warnings (Off, PolyORB.Setup.Server);

with PolyORB.ORB_Controller.Half_Sync_Half_Async;
pragma Warnings (Off, PolyORB.ORB_Controller.Half_Sync_Half_Async);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations);

with PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);

with PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);

with PolyORB.QoS.Priority;
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
   Rounded_Priority : RTCORBA.Priority;
   Ok : Boolean;
   myecho : Echo.Ref;

begin
   --  Note: per construction of this test, client and server must use
   --  the same PriorityMapping construct to ensure results consistency.

   if Argument_Count /= 1 then
      Put_Line ("usage : client <IOR_string_from_server>|-i");
      return;
   end if;

   New_Test ("CLIENT_PROPAGATED client");

   CORBA.ORB.Initialize ("ORB");

   --  Setting up default Priority Mapping for this node

   PolyORB.RTCORBA_P.Setup.Set_Priority_Mapping (Priority_Mapping);

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

   Output ("Running priority is correct",
           Running_Priority = CORBA.Short (0));
   --  Note: per construction of this test, 0 the expected result of
   --  the mapping of the CORBA priority onto the native priority as
   --  defined on the server side.

   declare
      Current : constant RTCORBA.Current.Local_Ref :=
                  RTCORBA.Current.Helper.To_Local_Ref
                    (Resolve_Initial_References
                      (To_CORBA_String ("RTCurrent")));

      Client_Priority_1 : constant RTCORBA.Priority := 10_000;
      Client_Priority_2 : constant RTCORBA.Priority := 20_000;

   begin
      --  Set up client priority

      Output ("Retrieve reference on RTCurrent", True);

      declare
         Priority : RTCORBA.Priority;
         pragma Unreferenced (Priority);

      begin
         Priority := RTCORBA.Current.Get_The_Priority (Current);
         Output ("Retrieve RTCurrent priority raised no exception", False);
      exception
         when CORBA.Initialize =>
            Output ("Retrieve unset RTCurrent priority raised "
                    & "CORBA.Initialize",
                    True);
      end;

      --  Test #1, invocation with priority Client_Priority_1

      RTCORBA.Current.Set_The_Priority (Current, Client_Priority_1);
      Output ("Set RTCurrent priority", True);

      Output ("New RTCurrent priority =" & Client_Priority_1'Img & " :",
              RTCORBA.Current.Get_The_Priority (Current) = Client_Priority_1);

      --  Computing rounded priority

      RTCORBA.PriorityMapping.To_CORBA
        (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
         RTCORBA.NativePriority (Get_Priority),
         Rounded_Priority,
         Ok);

      if not Ok then
         raise Program_Error;
      end if;

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

      Output ("Running priority is correct",
              Running_Priority = CORBA.Short (Rounded_Priority));

      --  Test #2, invocation with priority Client_Priority_2

      RTCORBA.Current.Set_The_Priority (Current, Client_Priority_2);
      Output ("Set RTCurrent priority", True);

      Output ("New RTCurrent priority =" & Client_Priority_2'Img & " :",
              RTCORBA.Current.Get_The_Priority (Current) = Client_Priority_2);

      --  Computing rounded priority

      RTCORBA.PriorityMapping.To_CORBA
        (PolyORB.RTCORBA_P.Setup.Get_Priority_Mapping.all,
         RTCORBA.NativePriority (Get_Priority),
         Rounded_Priority,
         Ok);

      if not Ok then
         raise Program_Error;
      end if;

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

      Output ("Running priority is correct",
              Running_Priority = CORBA.Short (Rounded_Priority));

   exception
      when E : others =>
         New_Line;
         Put_Line ("Got exception: "
                   & Ada.Exceptions.Exception_Information (E));
         Output ("FATAL Error", False);
         raise;
   end;

   End_Report;
   CORBA.ORB.Shutdown (False);

exception
   when E : CORBA.Transient =>
      declare
         Memb : CORBA.System_Exception_Members;
      begin
         CORBA.Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (CORBA.Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (CORBA.Completion_Status'Image (Memb.Completed));

         End_Report;
      end;
end Client;
