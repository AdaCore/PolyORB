------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . U T I L S . R E P O R T                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
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

with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;

package body PolyORB.Utils.Report is

   Max : constant Natural := 60;
   Passed : Boolean := True;

   ------------
   -- Output --
   ------------

   procedure Output (Message : String; Result : Boolean) is
      Line : String (1 .. Max) := (others => '.');
      Last : Natural := Message'Length;
   begin
      if Last > Max then
         Last := Max;
      end if;

      Line (1 .. Last) := Message (Message'First .. Message'First + Last - 1);

      if Result then
         Ada.Text_IO.Put_Line (Line & ": PASSED");
      else
         Ada.Text_IO.Put_Line (Line & ": FAILED");
      end if;

      Passed := Passed and then Result;
   end Output;

   ----------------
   -- End_Report --
   ----------------

   procedure End_Report is
   begin
      Output ("END TESTS", Passed);
   end End_Report;

   --------------
   -- New_Test --
   --------------

   procedure New_Test (Test_Name : String) is
   begin
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("==> Begin test " & Test_Name & " <==");
   end New_Test;

   package body Statistics is

      use Ada.Text_IO;

      ---------
      -- Min --
      ---------

      function Min (V : Stat_Vector) return T is
         Result : T := V (V'First);

      begin
         for J in V'Range loop
            if Result >  V (J) then
               Result := V (J);
            end if;
         end loop;

         return Result;
      end Min;

      ---------
      -- Max --
      ---------

      function Max (V : Stat_Vector) return T is
         Result : T := V (V'First);

      begin
         for J in V'Range loop
            if Result <  V (J) then
               Result := V (J);
            end if;
         end loop;

         return Result;
      end Max;

      ---------
      -- Avg --
      ---------

      function Avg (V : Stat_Vector) return Float is
         Result : Float := 0.0;

      begin
         for J in V'Range loop
            Result := Result + Float (V (J));
         end loop;

         if V'Length > 0 then
            return Result / Float (V'Length);

         else
            return Result;
         end if;
      end Avg;

      -------------
      -- Std_Dev --
      -------------

      function Std_Dev (V : Stat_Vector) return Float is
         use Ada.Numerics.Elementary_Functions;

         Result : Float := 0.0;

         Mean : constant Float := Avg (V);

      begin
         if V'Length < 2 then
            raise Program_Error;
         end if;

         for J in V'Range loop
            Result := Result + (Float (V (J)) - Mean) ** 2;
         end loop;

         Result := Sqrt (Result / Float (V'Length - 1));

         return Result;
      end Std_Dev;

      ---------------
      -- Partition --
      ---------------

      function Partition
        (V : Stat_Vector;
         Number_Of_Bins : Natural;
         Low : Float;
         High : Float)
        return Partitions
      is
         Result : Partitions (0 .. Number_Of_Bins + 1);
         It : Natural := 0;
         Done : Natural := 0;

      begin
         if V'Length = 0 then
            raise Program_Error;
         end if;

         for K in 0 .. Number_Of_Bins + 1 loop
            Result (K).Index
              := T (Low + Float (K) * (High - Low) / Float (Result'Length));
         end loop;

         for J in V'Range loop
            It := It + 1;
            for K in Result'Range loop
               if Float (V (J)) <=
                 Low + Float (K) * (High - Low) / Float (Result'Length) then
                  Result (K).Value := Result (K).Value + 1;
                  Done := Done + 1;
                  exit;
               end if;
            end loop;
         end loop;

         return Result;
      end Partition;

      ----------------
      -- To_GNUPlot --
      ----------------

      procedure To_GNUPlot (V : Stat_Vector; Filename : String) is
         Output_FH : Ada.Text_IO.File_Type;

      begin
         Create (Output_FH, Out_File, Filename & ".gnuplot");

         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "# GNUPlot configuration");
         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "set size 1.0, 0.4");
         Put_Line (Output_FH, "set grid");
         Put_Line (Output_FH, "set terminal postscript eps "
                   & "enhanced colour lw 2 ""Helvetica"" 14");
         Put_Line (Output_FH, "set out """ & Filename & ".eps""");
         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "# Data To Be Plotted");
         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "#Min:" & T'Image (Min (V)));
         Put_Line (Output_FH, "#Max:" & T'Image (Max (V)));
         Put_Line (Output_FH, "#Max - Min:" & T'Image (Max (V) - Min (V)));
         Put_Line (Output_FH, "#Avg:" & Float'Image (Avg (V)));
         Put_Line (Output_FH, "#Dev:" & Float'Image (Std_Dev (V)));
         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "plot ""-"" notitle");

         for J in V'Range loop
            Put_Line (Output_FH, T'Image (V (J)));
         end loop;

         Put_Line (Output_FH, "end");

         Close (Output_FH);
      end To_GNUPlot;

      procedure To_GNUPlot (P : Partitions; Filename : String) is
         Output_FH : Ada.Text_IO.File_Type;

      begin
         Create (Output_FH, Out_File, Filename & ".gnuplot");

         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "# GNUPlot configuration");
         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "set size 1.0, 0.4");
         Put_Line (Output_FH, "set grid");
         Put_Line (Output_FH, "set terminal postscript eps "
                   & "enhanced colour lw 2 ""Helvetica"" 14");
         Put_Line (Output_FH, "set data style linespoints");
         Put_Line (Output_FH, "set out """ & Filename & ".eps""");
         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "# Data To Be Plotted");
         Put_Line (Output_FH, "#");
         Put_Line (Output_FH, "plot ""-"" notitle");

         for J in P'Range loop
            Put_Line (Output_FH, T'Image (P (J).Index)
                      & Natural'Image (P (J).Value));
         end loop;

         Put_Line (Output_FH, "end");

         Close (Output_FH);
      end To_GNUPlot;

      --------------------
      -- Analyse_Vector --
      --------------------

      procedure Analyse_Vector (V : Stat_Vector; Filename : String) is
         P : constant Partitions
           := Partition (V, 100, 0.9 * Avg (V), 1.1 * Avg (V));
      begin
         Put_Line ("Output data for " & Filename);
         Put_Line ("   Min:" & T'Image (Min (V)));
         Put_Line ("   Max:" & T'Image (Max (V)));
         Put_Line ("   Avg:" & Float'Image (Avg (V)));
         Put_Line ("   Dev:" & Float'Image (Std_Dev (V)));

         To_GNUPlot (V, Filename);
         To_GNUPlot (P, Filename & "_bins");
      end Analyse_Vector;

   end Statistics;

end PolyORB.Utils.Report;
