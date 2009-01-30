------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ D R I V E R                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2009, Free Software Foundation, Inc.          --
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

--  Wrapper to launch PolyORB's testsuite

with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Source_Info;

with Test_Suite.Scenarios;
with Test_Suite.Output.File;
with Test_Suite.Output.Text;

with PolyORB.Initialization;

with PolyORB.Parameters.Initialization;
pragma Warnings (Off, PolyORB.Parameters.Initialization);

with PolyORB.Log.Stderr;
pragma Warnings (Off, PolyORB.Log.Stderr);

procedure Test_Driver is

   use Ada.Text_IO;
   use GNAT.Command_Line;

   use Test_Suite.Output;
   use Test_Suite.Output.File;
   use Test_Suite.Output.Text;

   procedure Run;
   --  Run test driver

   procedure Scan_Command_Line;
   --  Scan the command line

   procedure Usage;
   --  Print usage information

   type String_Access is access all String;

   type Action is (Run_Scenario,
                   Run_All_Scenarios);

   Scan_Succesful : Boolean := False;
   To_Do          : Action;
   Output         : TSO_Access;
   Item           : String_Access;
   Configuration_Base_Dir : String_Access;
   Position : Integer := -1;
   Verbose : Boolean := False;

   ---------
   -- Run --
   ---------

   procedure Run is
      Result : Boolean;
   begin
      --  Execute test_driver

      Open (Test_Suite_Output'Class (Output.all));
      Log (Test_Suite_Output'Class (Output.all), "Test driver launched.");

      case To_Do is
         when Run_Scenario =>
            Test_Suite.Scenarios.Run_Scenario
              (Item.all, Position,
               Configuration_Base_Dir.all,
               Test_Suite_Output'Class (Output.all),
               Result,
               Verbose);

         when Run_All_Scenarios =>
            Test_Suite.Scenarios.Run_All_Scenarios
              (Item.all,
               Configuration_Base_Dir.all,
               Test_Suite_Output'Class (Output.all),
               Result,
               Verbose);
      end case;

      Log (Test_Suite_Output'Class (Output.all), "Test driver exited.");
      if Result then
         Log (Test_Suite_Output'Class (Output.all), "No test failed.");
      else
         Log (Test_Suite_Output'Class (Output.all), "Some tests failed.");
      end if;

      Close (Test_Suite_Output'Class (Output.all));

      if not Result then
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end Run;

   -----------------------
   -- Scan_Command_Line --
   -----------------------

   procedure Scan_Command_Line is
   begin
      loop
         case Getopt ("scenario: full: output: config: position: verbose") is
            when ASCII.NUL =>
               exit;

            when 'c' =>
               if Full_Switch = "config" then
                  Configuration_Base_Dir := new String '(Parameter);
               end if;

            when 'f' =>
               if Full_Switch = "full" then
                  To_Do := Run_All_Scenarios;
                  Item := new String '(Parameter);
                  Scan_Succesful := True;
               end if;

            when 'o' =>
               if Full_Switch = "output" then
                  if Parameter = "stdout" then
                     Output := new Text_Output;

                  elsif Parameter = "file" then
                     Output := new File_Output;

                  else
                     Put_Line (Standard_Error, "Invalid output: " & Parameter);
                     raise Constraint_Error;
                  end if;
               end if;

            when 's' =>
               if Full_Switch = "scenario" then
                  To_Do := Run_Scenario;
                  Item := new String '(Parameter);
                  Scan_Succesful := True;
               end if;

            when 'p' =>
               if Full_Switch = "position" then
                  Position := Integer'Value (Parameter);
               end if;

            when 'v' =>
               if Full_Switch = "verbose" then
                  Verbose := True;
               end if;

            when others =>
               raise Program_Error;
         end case;
      end loop;

   exception
      when Invalid_Switch =>
         Scan_Succesful := False;
         Put_Line (Standard_Error, "Invalid Switch " & Full_Switch);

      when Invalid_Parameter =>
         Scan_Succesful := False;
         Put_Line (Standard_Error, "No parameter for " & Full_Switch);
   end Scan_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      Filename        : constant String := GNAT.Source_Info.File;
      Executable_Name : constant String :=
                          Filename (Filename'First .. Filename'Last - 4);

   begin
      New_Line;
      Put_Line (Standard_Error, "Usage: " & Executable_Name
                  & " -scenario scenario_file [-position N]"
                  & "|-full directory"
                  & " -output file|stdout -config dir,"
                  & " -verbose");
      Put_Line (Standard_Error,
                "  -scenario scenario_file : plays scenario_file,");
      Put_Line (Standard_Error,
                "  -position N             : plays only test #N");
      Put_Line (Standard_Error,
                "  -full     directory     : plays all scenarios" &
                " in directory.");
      Put_Line (Standard_Error,
                "  -output   file|stdout   : output to files|standard output");
      Put_Line (Standard_Error,
                "  -config   dir           : directory for scenario files ");
      Put_Line (Standard_Error,
                "  -verbose                : print information on the run ");

      New_Line;
   end Usage;

   --  Main procedure begins here

begin
   PolyORB.Initialization.Initialize_World;

   Scan_Command_Line;

   if Scan_Succesful and then Output /= null then
      Run;
   else
      Usage;
      GNAT.OS_Lib.OS_Exit (1);
   end if;
end Test_Driver;
