------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          B E N C H _ U T I L S                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2007-2012, Free Software Foundation, Inc.          --
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

with Ada.Calendar;
with GNAT.IO_Aux;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;
with PolyORB.Utils.Report;
with GNAT.Calendar.Time_IO;
with GNAT.Regpat;

package body Bench_Utils is

   use Ada.Text_IO;
   use Ada.Real_Time;

   use PolyORB.Utils.Report;

   --------------
   -- Run_Test --
   --------------

   procedure Run_Test is
      Test_Filename : constant String := Test_Name & ".data";

      D1, D2, D3      : Time;
      D               : Duration;
      Fd : File_Type;
      Request_Sent_In_Previous_Test : Natural := 0;
      Line : String (1 .. 1_024);
      Last : Integer;

   begin
      New_Test ("CORBA : " & Test_Name & " function");

      if GNAT.IO_Aux.File_Exists (Test_Filename) then
         Open (Fd, In_File, Test_Filename);

         while not End_Of_File (Fd) loop
            Get_Line (Fd, Line, Last);

            --  Fetch value from previous test run

            declare
               use GNAT.Regpat;

               Regexp : constant String
                 := "([[:ascii:]]+)  ([[:ascii:]]+)  ([[:ascii:]]+)";
               Matches : Match_Array (0 .. 3);
            begin
               Match (Compile (Regexp), Line (Line'First .. Last), Matches);

               Request_Sent_In_Previous_Test
                 := Natural'Value
                 (Line (Matches (3).First .. Matches (3).Last));
            end;
         end loop;

         --  We completed read from File, we now reopen it in Append
         --  mode to addd result from the next run.

         Close (Fd);
         Open (Fd, Append_File, Test_Filename);

      else
         --  Create new data file

         Create (Fd, Out_File, Test_Filename);
         Request_Sent_In_Previous_Test := 0;
      end if;

      declare
         Cpt : Natural := 0;
         X   : Integer := 0;
      begin
         --  Run test during at least Test_Duration

         begin
            D1 := Clock;
            while True loop
               Test;
               Cpt := Cpt + 1;
               D2 := Clock;
               X := Integer (To_Duration (D2 - D1));
               if (((Cpt + 1) * X) / Cpt) >= Test_Duration then
                  exit;
               end if;
            end loop;
         exception
            when E : others =>
               Put_Line ("Got exception: "
                         & Ada.Exceptions.Exception_Information (E));
               Output (Test_Name, False);
               Close (Fd);
               return;
         end;

         D2 := Clock;
         D3 := Clock;
         D := To_Duration (D2 - (D3 - D2) - D1);

         --  The test was successful

         Output (Test_Name, True);

         --  Process test benchs

         --  1/ Print the execution time average and the number of
         --  executed requests

         Put_Line
           (Fd,
            GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock,
                                         "%y/%m/%d:%Hh%M")
            & "       " &
            Float'Image (Float (D) / Float (Cpt)) &
            "       " &
            Natural'Image (Cpt));
         Put_Line
           (GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock,
                                         "%y/%m/%d:%Hh%M")
            & "       " &
            Float'Image (Float (D) / Float (Cpt)) &
            "       " &
            Natural'Image (Cpt));
         Close (Fd);

         --  2/ Test performance against previous value

         if Float (Request_Sent_In_Previous_Test) - Float (Cpt)
           > Threshold * Float (Cpt)
         then
            Output ("Performance decreased, old value was"
                    & Natural'Image (Request_Sent_In_Previous_Test), False);
         end if;
      end;
   end Run_Test;

end Bench_Utils;
