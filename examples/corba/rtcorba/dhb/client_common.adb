------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C L I E N T _ C O M M O N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with GNAT.Calendar.Time_IO;

with CORBA.ORB;

with RTCORBA.PriorityMapping.Linear;

with PolyORB.RTCORBA_P.Setup;
with PolyORB.Utils.Report;

with Dyn_Dict;
with DHB;
with Periodic_Clients;
with Sporadic_Clients;
with Whetstone;

package body Client_Common is

   ----------------
   -- Run_Client --
   ----------------

   procedure Run_Client is
      use Ada.Real_Time;

      use PolyORB.Utils.Report;

      Worker_String_Ref : constant CORBA.String
        := CORBA.To_CORBA_String ("file://worker.ior");

      Background_Worker_String_Ref : constant CORBA.String
        := CORBA.To_CORBA_String ("file://background_worker.ior");

      Priority_Mapping : RTCORBA.PriorityMapping.Linear.Object;

      Timestamp : constant String := GNAT.Calendar.Time_IO.Image
     (Ada.Calendar.Clock, "%s") & "-";

   begin
      New_Test ("DHB client");

      CORBA.ORB.Initialize ("ORB");

      --  Setting up default Priority Mapping for this node

      PolyORB.RTCORBA_P.Setup.Set_Priority_Mapping (Priority_Mapping);

      --  Timestamp

      Output ("Timestamp: " & Timestamp, True);

      --  Computing node KWIPS

      Output ("This node KWIPS ="
              & Positive'Image (Whetstone.Compute_KWIPS), True);

      --  Computing error on the clock

      Output ("Clock error=" & Duration'Image (To_Duration (Clock - Clock)),
              True);

      --  Local Test #1

      Dyn_Dict.Test_Register (Timestamp, 1_000);
      delay 1.0;

      --  Periodic Test #1

      Periodic_Clients.Run_Test_1
        ((1 => Periodic_Clients.Periodic_Task_Information'
          (Id                        => 0,
           Worker_String_Ref         => Worker_String_Ref,
           Client_Priority           => RTCORBA.Priority'(10_000),
           Client_Workload           => 10,
           Initial_Server_Workload   => DHB.KWIPS'(10),
           Server_Workload_Increment => DHB.KWIPS'(100),
           Period                    => Ada.Real_Time.To_Time_Span (0.1)),
          2 => Periodic_Clients.Periodic_Task_Information'
          (Id                        => 1,
           Worker_String_Ref         => Worker_String_Ref,
           Client_Priority           => RTCORBA.Priority'(10_000),
           Client_Workload           => 10,
           Initial_Server_Workload   => DHB.KWIPS'(10),
           Server_Workload_Increment => DHB.KWIPS'(100),
           Period                    => Ada.Real_Time.To_Time_Span (0.05))));
      delay 1.0;

      --  Periodic Test #2

      Periodic_Clients.Run_Test_2 (Worker_String_Ref);
      delay 1.0;

      --  Sporadic Test #1

      Sporadic_Clients.Run_Test_1
        (Stamp => Timestamp, Worker_String_Ref => Worker_String_Ref,
         How_Many => 1_000);
      delay 1.0;

      --  Sporadic Test #1b

      Sporadic_Clients.Run_Test_1b
        (Stamp                        => Timestamp,
         Worker_String_Ref            => Worker_String_Ref,
         Worker_Priority              => 1_000,
         Background_Worker_String_Ref => Background_Worker_String_Ref,
         Background_Worker_Priority   => 10_000,
         How_Many                     => 1_000);
      delay 1.0;

      --  Sporadic Test #2

      Sporadic_Clients.Run_Test_2
        (Stamp => Timestamp, Worker_String_Ref => Worker_String_Ref,
         How_Many => 1_000);
      delay 1.0;

      --  Sporadic Test #3

      Sporadic_Clients.Run_Test_3
        (Stamp => Timestamp, Worker_String_Ref => Worker_String_Ref,
         How_Many => 10, Iterations => 3);

      End_Report;

      CORBA.ORB.Shutdown (Wait_For_Completion => False);

   exception
      when E : others =>
         Output ("Got exception !", False);
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Run_Client;

end Client_Common;
