------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ D R I V E R                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2003 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Wrapper to launch PolyORB's testsuite.

--  $Id$

with Ada.Exceptions;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Source_Info;

with Test_Suite.Scenarios;
with Test_Suite.Output.File;

procedure Test_Driver is

   use Ada.Text_IO;
   use GNAT.Command_Line;

   use Test_Suite.Output.File;

   procedure Usage;
   --  Print usage information.

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
                & " -scenario scenario_file|full directory,");
      Put_Line (Standard_Error,
                "  -scenario scenario_file : plays scenario_file,");
      Put_Line (Standard_Error,
                "  -full     directory     : plays all scenarios" &
                " in directory.");
      New_Line;
   end Usage;

   type Action is (Run_Scenario,
                   Run_All_Scenarios);

   Scan_Succesful : Boolean := False;

   To_Do : Action;
   Output : File_Output;

   --  Main procedure begins here.

begin

   --  Scan the command line.
   loop
      case Getopt ("scenario: full:") is
         when ASCII.NUL =>
            exit;

         when 's' =>
            if Full_Switch = "scenario" then
               To_Do := Run_Scenario;
               Scan_Succesful := True;
            end if;

         when 'f' =>
            if Full_Switch = "full" then
               To_Do := Run_All_Scenarios;
               Scan_Succesful := True;
            end if;

         when others =>
            raise Program_Error;
      end case;
   end loop;

   --  Print usage if scan is unsuccesful.
   if not Scan_Succesful then
      Usage;
      return;
   end if;

   --  Execute test_driver ..
   Open (Output);
   Log (Output, "Test driver launched.");

   case To_Do is
      when Run_Scenario =>
         Test_Suite.Scenarios.Run_Scenario (Parameter, 1, Output);

      when Run_All_Scenarios =>
         Test_Suite.Scenarios.Run_All_Scenarios (Parameter, Output);
   end case;

   Log (Output, "Test driver exited.");
   Close (Output);

exception
   when Invalid_Switch =>
      Put_Line (Standard_Error, "Invalid Switch " & Full_Switch);
      Usage;

   when Invalid_Parameter =>
      Put_Line (Standard_Error, "No parameter for " & Full_Switch);
      Usage;

   when E : others =>
      Put_Line (Standard_Error, "==> Internal Error <==");
      Put_Line (Standard_Error,
                " Got exception: "
                & Ada.Exceptions.Exception_Name (E)
                & ", "
                & Ada.Exceptions.Exception_Message (E));

end Test_Driver;
