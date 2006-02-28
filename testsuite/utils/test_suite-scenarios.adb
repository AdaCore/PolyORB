------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T _ S U I T E . S C E N A R I O S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with GNAT.Directory_Operations.Iteration;
with PolyORB.Parameters.File;

with Test_Suite.Test_Case.Parser;

package body Test_Suite.Scenarios is

   Total_Failed_Tests : Natural := 0;
   Total_Tests : Natural := 0;

   ------------------
   -- Run_Scenario --
   ------------------

   procedure Run_Scenario
     (Scenario_File : String;
      Index         : Positive;
      Configuration_Dir : String;
      Output        : Test_Suite_Output'Class)
   is
      use PolyORB.Parameters;
      use PolyORB.Parameters.File;

      use Test_Suite.Test_Case.Parser;
      use Test_Suite.Test_Case;

   begin
      Load_Configuration_File (Scenario_File);

      declare
         Scenario_Name : constant String := Get_Conf ("scenario", "name");

         Scenario_Id : constant String := Get_Conf ("scenario", "id");

         Count : Natural := 0;
         Failed_Tests : Natural := 0;

         Result_Total : Boolean := True;
         Result : Boolean;

      begin
         if Scenario_Name = "" then
            Log (Output, "Invalid scenario name in file: " & Scenario_File);
            raise Program_Error;
         end if;

         Open_Scenario_Output_Context (Output, Scenario_Name);

         Log (Output, "Scenario #"
              & Positive'Image (Index)
              & ": "
              & Scenario_Name);
         Log (Output, "Description: " & Scenario_Id);

         loop
            declare
               Extracted_Test : Test'Class
                 := Extract_Test
                 (Scenario_Name, Count, Configuration_Dir, Output);

            begin
               exit when Extracted_Test in Null_Test;
               Count := Count + 1;

               Result := Run_Test (Extracted_Test, Output);

               if not Result then
                  Failed_Tests := Failed_Tests + 1;
               end if;

               Result_Total := Result_Total and Result;

               delay 1.0;
            end;
         end loop;

         if Failed_Tests = 0 then
            Log (Output, "PASSED: all"
                 & Natural'Image (Count) & " tests passed");
         else
            Log (Output, "FAILED:"
                 & Natural'Image (Count - Failed_Tests)
                 & " out of" & Natural'Image (Count) & " tests passed");
         end if;

         Separator (Output);

         Close_Scenario_Output_Context (Output, Result_Total);

         Total_Failed_Tests := Total_Failed_Tests + Failed_Tests;
         Total_Tests := Total_Tests + Count;
      end;

   exception
      when E : others =>
         Log (Output, "Error in scenario file: " & Scenario_File);
         Log (Output, Ada.Exceptions.Exception_Information (E));
         Separator (Output);
   end Run_Scenario;

   -----------------------
   -- Run_All_Scenarios --
   -----------------------

   procedure Run_All_Scenarios
     (Directory_Name : String;
      Configuration_Dir : String;
      Output         : Test_Suite_Output'Class)
   is
      Scenarios : Natural := 0;

      procedure Run_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean);

      procedure Run_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean) is
      begin
         Run_Scenario (Scenario_File, Index, Configuration_Dir, Output);
         Scenarios := Scenarios + 1;
         Quit := False;
      end Run_Scenario_Wrapper;

      procedure Run_Scenario_With_Pattern is new
        GNAT.Directory_Operations.Iteration.Find (Run_Scenario_Wrapper);

   begin
      Log (Output, "Running all scenario from: " & Directory_Name);
      Separator (Output);

      Run_Scenario_With_Pattern (Directory_Name, "(.*)-(.*)\.conf");
      Log (Output, Natural'Image (Scenarios) & " scenarios executed,");
      Log (Output, Natural'Image (Total_Tests - Total_Failed_Tests)
           & " out of" & Natural'Image (Total_Tests)
           & " tests passed");
   end Run_All_Scenarios;

end Test_Suite.Scenarios;
