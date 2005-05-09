------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ D R I V E R                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
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

--  Wrapper to launch PolyORB's testsuite.

with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Source_Info;

with Test_Suite.Scenarios;
with Test_Suite.Output.File;
with Test_Suite.Output.Text;

with PolyORB.Initialization;

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

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      --  Execute test_driver ..
      Open (Test_Suite_Output'Class (Output.all));
      Log (Test_Suite_Output'Class (Output.all), "Test driver launched.");

      case To_Do is
         when Run_Scenario =>
            Test_Suite.Scenarios.Run_Scenario
              (Item.all, 1,
               Test_Suite_Output'Class (Output.all));

         when Run_All_Scenarios =>
            Test_Suite.Scenarios.Run_All_Scenarios
              (Item.all,
               Test_Suite_Output'Class (Output.all));
      end case;

      Log (Test_Suite_Output'Class (Output.all), "Test driver exited.");
      Close (Test_Suite_Output'Class (Output.all));

   exception
      when E : others =>
         Error (Test_Suite_Output'Class (Output.all),
                "==> Internal Error <==");
         Error (Test_Suite_Output'Class (Output.all),
                " Got exception: "
                & Ada.Exceptions.Exception_Name (E)
                & ", "
                & Ada.Exceptions.Exception_Message (E));
         Error (Test_Suite_Output'Class (Output.all),
                " with information: "
                & Ada.Exceptions.Exception_Information (E));
         Close (Test_Suite_Output'Class (Output.all));
   end Run;

   -----------------------
   -- Scan_Command_Line --
   -----------------------

   procedure Scan_Command_Line
   is
      No_Output : exception;

   begin
      loop
         case Getopt ("scenario: full: output:") is
            when ASCII.NUL =>
               exit;

            when 's' =>
               if Full_Switch = "scenario" then
                  To_Do := Run_Scenario;
                  Item := new String '(Parameter);
                  Scan_Succesful := True;
               end if;

            when 'f' =>
               if Full_Switch = "full" then
                  To_Do := Run_All_Scenarios;
                  Item := new String '(Parameter);
                  Scan_Succesful := True;
               end if;

            when 'o' =>
               if Full_Switch = "output" then
                  if Parameter = "text" then
                     Output := new Text_Output;
                  elsif Parameter = "file" then
                     Output := new File_Output;
                  else
                     raise No_Output;
                  end if;
               end if;

            when others =>
               raise Program_Error;
         end case;
      end loop;

   exception
      when Invalid_Switch =>
         Put_Line (Standard_Error, "Invalid Switch " & Full_Switch);

      when Invalid_Parameter =>
         Put_Line (Standard_Error, "No parameter for " & Full_Switch);

      when No_Output =>
         Put_Line (Standard_Error, "No output defined.");

   end Scan_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage
   is
      Filename : constant String := GNAT.Source_Info.File;
      Executable_Name : constant String
        := Filename (Filename'First .. Filename'Last - 4);

   begin
      New_Line;
      Put_Line (Standard_Error, "Usage: " & Executable_Name
                & " -scenario scenario_file|-full directory"
                & " -output file|text,");
      Put_Line (Standard_Error,
                "  -scenario scenario_file : plays scenario_file,");
      Put_Line (Standard_Error,
                "  -full     directory     : plays all scenarios" &
                " in directory.");
      Put_Line (Standard_Error,
                "  -output   file|text     : output to stdout or files");
      New_Line;
   end Usage;

   --  Main procedure begins here

begin
   PolyORB.Initialization.Initialize_World;

   Scan_Command_Line;

   if Scan_Succesful
     and then Output /= null then
      Run;
   else
      Usage;
   end if;

exception
   when E : others =>
      Put_Line (Standard_Error, "==> Internal Error <==");
      Put_Line (Standard_Error, " Got exception: "
                & Ada.Exceptions.Exception_Name (E)
                & ", "
                & Ada.Exceptions.Exception_Message (E));

end Test_Driver;
