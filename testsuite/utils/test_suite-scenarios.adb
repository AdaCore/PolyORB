------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T _ S U I T E . S C E N A R I O S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2009, Free Software Foundation, Inc.          --
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
with Ada.Text_IO;

with GNAT.Directory_Operations.Iteration;
with PolyORB.Parameters.File;
with PolyORB.Utils.Report;

with Test_Suite.Test_Case.Parser;

package body Test_Suite.Scenarios is

   use Ada.Text_IO;

   Total_Failed_Tests : Natural := 0;
   Total_Tests : Natural := 0;

   ------------------
   -- Run_Scenario --
   ------------------

   procedure Run_Scenario
     (Scenario_File     : String;
      Position          : Integer := -1;
      Configuration_Dir : String;
      Output            : Test_Suite_Output'Class;
      Test_Success      : out Boolean;
      Verbose           : Boolean)
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
         Expected_Failed_Tests : Natural := 0;

         Result_Total : Boolean := True;
         Result : Boolean;

      begin
         if Scenario_Name = "" then
            Log (Output, "Invalid scenario name in file: " & Scenario_File);
            raise Program_Error;
         end if;

         Open_Scenario_Output_Context (Output, Scenario_Name);

         Log (Output, "Scenario " & Scenario_Name);
         Log (Output, "Description: " & Scenario_Id);

         if Verbose then
            Put_Line ("Starting scenario " & Scenario_Name);
         end if;

         if Position = -1 then
            loop
               declare
                  Extracted_Test : constant Test'Class
                    := Extract_Test
                    (Scenario_Name, Count, Configuration_Dir, Output);

               begin
                  exit when Extracted_Test in Null_Test;
                  Count := Count + 1;

                  Result := Run_Test (Extracted_Test, Output);

                  if not Result then
                     if not Extracted_Test.Expected_Failure then
                        Failed_Tests := Failed_Tests + 1;
                     else
                        Expected_Failed_Tests := Expected_Failed_Tests + 1;
                     end if;
                  end if;

                  Result_Total := Result_Total
                    and (Result xor Extracted_Test.Expected_Failure);

                  delay 1.0;
               end;
            end loop;
         else
            declare
               Extracted_Test : constant Test'Class
                 := Extract_Test
                 (Scenario_Name, Position, Configuration_Dir, Output);

            begin
               pragma Assert (not (Extracted_Test in Null_Test));

               Result := Run_Test (Extracted_Test, Output);

               if not Result then
                  if not Extracted_Test.Expected_Failure then
                     Failed_Tests := Failed_Tests + 1;
                  else
                     Expected_Failed_Tests := Expected_Failed_Tests + 1;
                  end if;
               end if;

               Result_Total := Result_Total
                 and (Result xor Extracted_Test.Expected_Failure);

            end;
         end if;

         if Failed_Tests = 0 then
            Log (Output, "PASSED: all"
                 & Natural'Image (Count) & " tests passed, with"
                 & Natural'Image (Expected_Failed_Tests)
                 & " expected failed tests");
            Test_Success := True;

         else
            Log (Output, "FAILED:"
                 & Natural'Image (Count - Failed_Tests)
                 & " out of" & Natural'Image (Count) & " tests passed, with"
                 & Natural'Image (Expected_Failed_Tests)
                 & " expected failed tests");
            Test_Success := False;
         end if;

         if Verbose then
            PolyORB.Utils.Report.Output
              ("Scenario " & Scenario_Name, Test_Success);
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
     (Directory_Name    : String;
      Configuration_Dir : String;
      Output            : Test_Suite_Output'Class;
      Test_Success      : out Boolean;
      Verbose           : Boolean)
   is
      Scenarios : Natural := 0;

      procedure Count_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean);

      procedure Count_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean)
      is
         pragma Unreferenced (Scenario_File, Index);
      begin
         Scenarios := Scenarios + 1;
         Quit := False;
      end Count_Scenario_Wrapper;

      procedure Run_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean);

      procedure Run_Scenario_Wrapper
        (Scenario_File : String;
         Index         : Positive;
         Quit          : in out Boolean)
      is
         pragma Unreferenced (Index);
         Test_Output : Boolean;
      begin
         Run_Scenario
           (Scenario_File, -1, Configuration_Dir, Output, Test_Output,
            Verbose);
         Test_Success := Test_Success and Test_Output;
         Quit := False;
      end Run_Scenario_Wrapper;

      procedure Run_Scenario_With_Pattern is new
        GNAT.Directory_Operations.Iteration.Find (Run_Scenario_Wrapper);

      procedure Count_Scenario is new
        GNAT.Directory_Operations.Iteration.Find (Count_Scenario_Wrapper);

   begin
      Test_Success := True;

      Log (Output, "Running all scenario from: " & Directory_Name);
      Separator (Output);

      Count_Scenario (Directory_Name, "(.*)-(.*)\.conf");

      if Verbose then
         Put_Line ("Running all" & Integer'Image (Scenarios)
                     &  " scenario files from: " & Directory_Name);
      end if;

      Run_Scenario_With_Pattern (Directory_Name, "(.*)-(.*)\.conf");
      Log (Output, Natural'Image (Scenarios) & " scenarios executed,");
      Log (Output, Natural'Image (Total_Tests - Total_Failed_Tests)
             & " out of" & Natural'Image (Total_Tests)
             & " tests passed");

      if Verbose then
         Put_Line (Natural'Image (Scenarios) & " scenarios executed,");
         Put_Line (Natural'Image (Total_Tests - Total_Failed_Tests)
                     & " out of" & Natural'Image (Total_Tests)
                     & " tests passed");
      end if;
   end Run_All_Scenarios;

end Test_Suite.Scenarios;
