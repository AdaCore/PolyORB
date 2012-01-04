------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            R T C U R R E N T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.ORB;

with RTCORBA.Current.Helper;

with RTCORBA.PriorityMapping.Linear;

with PolyORB.RTCORBA_P.Setup;

with PolyORB.ORB.Thread_Pool;
pragma Warnings (Off, PolyORB.ORB.Thread_Pool);

with PolyORB.Setup.Server;
pragma Warnings (Off, PolyORB.Setup.Server);

with PolyORB.ORB_Controller.Workers;
pragma Warnings (Off, PolyORB.ORB_Controller.Workers);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Annotations);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads.Dynamic_Priorities);

with PolyORB.Tasking.Profiles.Full_Tasking.Threads;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Threads);

with PolyORB.Tasking.Profiles.Full_Tasking.Mutexes;
pragma Warnings (Off, PolyORB.Tasking.Profiles.Full_Tasking.Mutexes);

with PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables;
pragma Warnings
  (Off, PolyORB.Tasking.Profiles.Full_Tasking.Condition_Variables);

with PolyORB.Utils.Report;

procedure RTCurrent is

   use Ada.Text_IO;

   use CORBA.ORB;
   use RTCORBA;

   use PolyORB.Utils.Report;

   Priority_Mapping : RTCORBA.PriorityMapping.Linear.Object;

begin
   CORBA.ORB.Initialize ("ORB");

   --  Setting up default Priority Mapping for this node

   PolyORB.RTCORBA_P.Setup.Set_Priority_Mapping (Priority_Mapping);

   New_Test ("RTCurrent");

   declare
      Current : constant RTCORBA.Current.Local_Ref
        := RTCORBA.Current.Helper.To_Local_Ref
        (Resolve_Initial_References
         (To_CORBA_String ("RTCurrent")));

   begin
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

      RTCORBA.Current.Set_The_Priority (Current, 42);
      Output ("Set RTCurrent priority", True);

      Output ("New RTCurrent priority = 42 :",
              RTCORBA.Current.Get_The_Priority (Current) = 42);
      End_Report;
   exception
      when E : others =>
         New_Line;
         Put_Line ("Got exception: "
                   & Ada.Exceptions.Exception_Information (E));
         Output ("FATAL Error", False);
         End_Report;
   end;

   CORBA.ORB.Shutdown (False);
end RTCurrent;
