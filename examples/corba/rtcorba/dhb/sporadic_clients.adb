------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     S P O R A D I C _ C L I E N T S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2008, Free Software Foundation, Inc.          --
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

with Ada.Real_Time;
with Ada.Text_IO;

with CORBA.ORB;
with CORBA.Object.Policies;
with CORBA.Policy;

with RTCORBA.Current.Helper;

with DHB.Background_Worker;
with DHB.Worker;

with Utils;
with PolyORB.Types;
with PolyORB.Utils.Report;

package body Sporadic_Clients is
   use Ada.Real_Time;
   use Ada.Text_IO;

   use CORBA;
   use Utils;
   use Utils.Duration_Stats;
   use PolyORB.Utils.Report;

   ----------------
   -- Run_Test_1 --
   ----------------

   procedure Run_Test_1
     (Stamp             : Standard.String;
      Worker_String_Ref : CORBA.String;
      How_Many          : Positive)
   is
      Worker : DHB.Worker.Ref;

      Ok : Boolean := True;

      T0, T1, T2 : Ada.Real_Time.Time;

      Results : Stat_Vector (1 .. How_Many);

      Inconsistent_Policies : CORBA.Policy.PolicyList;

   begin
      New_Test ("Sporadic Test #1");
      Put_Line ("Description: Compute the Round Trip Time of a simple two-way "
                & "request:");
      Put_Line ("Call DHB.Worker.Round_Trip"
                & Integer'Image (How_Many) & " times");

      --  Getting the Worker object

      CORBA.ORB.String_To_Object (Worker_String_Ref, Worker);

      --  Checking if it worked

      if DHB.Worker.Is_Nil (Worker) then
         Put_Line ("Cannot invoke on a nil reference, exiting.");
         return;
      end if;

      CORBA.Object.Policies.Validate_Connection
        (CORBA.Object.Ref (Worker), Inconsistent_Policies, Ok);

      if not Ok then
         Put_Line ("No connection possible, exiting.");
         return;
      end if;

      --  Processing test

      for J in 1 .. How_Many loop
         T0 := Clock;
         Ok := Ok and (DHB.Worker.Round_Trip (Worker, 1234) = 1234);
         T1 := Clock;
         T2 := Clock;
         Results (J) := To_Duration (T1 - T0 - (T2 - T1));
      end loop;

      Analyse_Vector (Results (Results'First + 1 .. Results'Last),
                      Stamp & "sporadic_test_1");
      --  Do not take into account first value, it is biased by
      --  connection configuration.

      Output ("Test completed", True);
   end Run_Test_1;

   ----------------
   -- Run_Test_1b --
   ----------------

   procedure Run_Test_1b
     (Stamp                        : Standard.String;
      Worker_String_Ref            : CORBA.String;
      Worker_Priority              : RTCORBA.Priority;
      Background_Worker_String_Ref : CORBA.String;
      Background_Worker_Priority   : RTCORBA.Priority;
      How_Many                     : Positive)
   is
      use type DHB.KWIPS;

      Worker : DHB.Worker.Ref;
      Background_Worker : DHB.Background_Worker.Ref;

      Ok : Boolean := True;

      T0, T1, T2 : Ada.Real_Time.Time;

      Results : Stat_Vector (1 .. How_Many);
      Kilo_WIPS : DHB.KWIPS;

      Current : constant RTCORBA.Current.Local_Ref :=
                  RTCORBA.Current.Helper.To_Local_Ref
                    (CORBA.ORB.Resolve_Initial_References
                      (CORBA.ORB.To_CORBA_String ("RTCurrent")));

   begin
      New_Test ("Sporadic Test #1b");
      Put_Line ("Description: Compute the Round Trip Time of a simple two-way "
                & "request with perturbation from a Background_Worker:");
      Put_Line ("Call DHB.Worker.Round_Trip"
                & Integer'Image (How_Many) & " times");

      CORBA.ORB.String_To_Object (Worker_String_Ref, Worker);
      CORBA.ORB.String_To_Object (Background_Worker_String_Ref,
                                  Background_Worker);

      --  Get Background_Worker KWIPS

      Kilo_WIPS := DHB.Background_Worker.Get_KWIPS (Background_Worker);

      Output ("Background_Worker'KWIPS:" & DHB.KWIPS'Image (Kilo_WIPS), True);

      --  Set up Worker's RT-CORBA Priority

      RTCORBA.Current.Set_The_Priority (Current, Worker_Priority);

      --  Run Background Worker

      DHB.Background_Worker.Do_Background_Work
        (Background_Worker, Kilo_WIPS * 4, Background_Worker_Priority);

      --  Run Worker test

      for J in 1 .. How_Many loop
         T0 := Clock;
         Ok := Ok and (DHB.Worker.Round_Trip (Worker, 1234) = 1234);
         T1 := Clock;
         T2 := Clock;
         Results (J) := To_Duration (T1 - T0 - (T2 - T1));
      end loop;
      Output ("Done", Ok);

      Analyse_Vector (Results (Results'First + 1 .. Results'Last),
                      Stamp & "sporadic_test_1b");
      --  Do not take into account first value, it is biased by
      --  connection configuration.

      Output ("Test completed", True);
   end Run_Test_1b;

   ----------------
   -- Run_Test_2 --
   ----------------

   procedure Run_Test_2
     (Stamp             : Standard.String;
      Worker_String_Ref : CORBA.String;
      How_Many          : Positive)
   is
      Worker : DHB.Worker.Ref;

      T0, T1, T2 : Ada.Real_Time.Time;

      Results : Stat_Vector (1 .. How_Many);

      Ok : Boolean;
      Inconsistent_Policies : CORBA.Policy.PolicyList;
   begin
      New_Test ("Sporadic Test #2");
      Put_Line ("Description: Compute the Round Trip Time of a simple oneway "
                & "request:");
      Put_Line ("Call DHB.Worker.Ping"
                & Integer'Image (How_Many) & " times");

      --  Getting the Worker object

      CORBA.ORB.String_To_Object (Worker_String_Ref, Worker);

      --  Checking if it worked

      if DHB.Worker.Is_Nil (Worker) then
         Put_Line ("Cannot invoke on a nil reference, exiting.");
         return;
      end if;

      CORBA.Object.Policies.Validate_Connection
        (CORBA.Object.Ref (Worker), Inconsistent_Policies, Ok);

      if not Ok then
         Put_Line ("No connection possible, exiting.");
         return;
      end if;

      --  Processing test

      for J in 1 .. How_Many loop
         T0 := Clock;
         DHB.Worker.Ping (Worker, 1234);
         T1 := Clock;
         T2 := Clock;
         Results (J) := To_Duration (T1 - T0 - (T2 - T1));
      end loop;

      Analyse_Vector (Results (Results'First + 1 .. Results'Last),
                      Stamp & "sporadic_test_2");
      --  Do not take into account first value, it is biased by
      --  connection configuration.

      Output ("Test completed", True);
   end Run_Test_2;

   ----------------
   -- Run_Test_3 --
   ----------------

   procedure Run_Test_3
     (Stamp             : Standard.String;
      Worker_String_Ref : CORBA.String;
      How_Many          : Positive;
      Iterations        : Natural)
   is
      use type DHB.Worker.U_sequence;

      Worker : DHB.Worker.Ref;

      Ok : Boolean := True;

      Inconsistent_Policies : CORBA.Policy.PolicyList;

      T0, T1, T2 : Ada.Real_Time.Time;

      Results : Stat_Vector (1 .. How_Many);

      Seq : DHB.Worker.U_sequence
        := DHB.Worker.U_sequence
        (DHB.Worker.IDL_SEQUENCE_unsigned_long.Null_Sequence);

      BW : Stat_Vector (1 .. Iterations);
      RTT : Float;

   begin
      New_Test ("Sporadic Test #3");
      Put_Line ("Description: Compute the Bandwidth of the middleware:");
      Put_Line ("Call DHB.Worker.Round_Trip_With_Payload"
                & Integer'Image (How_Many) & " times, "
                & "increasing the payload");

      --  Getting the Worker object

      CORBA.ORB.String_To_Object (Worker_String_Ref, Worker);

      --  Checking if it worked

      if DHB.Worker.Is_Nil (Worker) then
         Put_Line ("Cannot invoke on a nil reference, exiting.");
         return;
      end if;

      CORBA.Object.Policies.Validate_Connection
        (CORBA.Object.Ref (Worker), Inconsistent_Policies, Ok);

      if not Ok then
         Put_Line ("No connection possible, exiting.");
         return;
      end if;

      --  Processing test

      for J in 0 .. Iterations loop
         for K in 1 .. How_Many loop
            T0 := Clock;
            Ok := Ok
              and (DHB.Worker.Round_Trip_With_Payload (Worker, Seq) = Seq);
            T1 := Clock;
            T2 := Clock;
            Results (K) := To_Duration (T1 - T0 - (T2 - T1));
         end loop;

         declare
            Results2 : Stat_Vector
              renames Results (Results'First + 1 .. Results'Last);
            --  Do not take into account first value, it is biased by
            --  connection configuration.
         begin
            if DHB.Worker.Length (Seq) = 0 then
               RTT := Avg (Results2);
               Put_Line ("RTT :" & Float'Image (RTT));

               Put_Line ("(Payload, Bandwidth (b/s))");

               Seq := DHB.Worker.To_Sequence (1024);
            else
               Put_Line
                 (Integer'Image (DHB.Worker.Length (Seq))
                  & Float'Image (Float (DHB.Worker.Length (Seq))
                                 * Float (CORBA.Unsigned_Long'Size)
                                 * 2.0
                                 / (Avg (Results2) - RTT)));

               BW (J) := Duration
                 (Float (DHB.Worker.Length (Seq))
                  * Float (CORBA.Unsigned_Long'Size)
                  * 2.0
                  / (Avg (Results2) - RTT));
            end if;

            Analyse_Vector (Results2,
                            Stamp & "sporadic_test_3_" &
                             PolyORB.Types.Trimmed_Image
                              (PolyORB.Types.Long_Long (J)));
         end;
         Seq := Seq & Seq;
      end loop;
      Analyse_Vector (BW, Stamp & "sporadic_test_3_bw");

      Output ("Test completed", True);
   end Run_Test_3;

end Sporadic_Clients;
