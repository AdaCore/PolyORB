------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ D R I V E R                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  Wrapper to lunch PolyORB's testsuite.

--  $Id$

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.Expect;
with GNAT.OS_Lib;

with PolyORB.Configuration;
with PolyORB.Log;

procedure Test_Driver is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use GNAT.Command_Line;
   use GNAT.OS_Lib;
   use GNAT.Directory_Operations;

   use PolyORB.Configuration;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("test_driver");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Test_Kind is (Local);

   Default_Timeout : constant Integer := 10_000;

   type Test_Case is record
      Id         : Ada.Strings.Unbounded.Unbounded_String;
      Test_Type  : Test_Kind := Local;
      Executable : Ada.Strings.Unbounded.Unbounded_String;
      Timeout    : Integer := 0;
   end record;

   Null_Test : constant Test_Case :=
     (Id => To_Unbounded_String (""),
      Test_Type => Local,
      Executable => To_Unbounded_String (""),
      Timeout => 0);

   function Read_Test
     (Scenario : String;
      Number   : Natural)
     return Test_Case;
   --  Extract test case #Number from scenario file.

   procedure Launch_Test (Test_To_Run : Test_Case);
   --  Launch test.

   procedure Run_Scenario;
   --  Run scenario file.

   procedure Usage;
   --  Print usage information.

   ---------------
   -- Read_Test --
   ---------------

   function Read_Test
     (Scenario : String;
      Number   : Natural)
     return Test_Case
   is
      Test_Id : constant String := Natural'Image (Number);
      Section : constant String
        := "test " & Scenario & "_"
        & Test_Id (Test_Id'First + 1 .. Test_Id'Last);

      Result : Test_Case;

      Id_S         : constant String := Get_Conf (Section, "id");
      Test_Type_S  : constant String := Get_Conf (Section, "type");
      Executable_S : constant String := Get_Conf (Section, "command");
   begin

      --  Does the test exist ?

      if Id_S = "" and then Test_Type_S = "" and then Executable_S = "" then
         return Null_Test;
      end if;

      O ("Read     : " & Section);
      O (" Id      : " & Id_S);
      O (" Type    : " & Test_Type_S);
      O (" Command : " & Executable_S);

      --  Test Id.
      Result.Id := To_Unbounded_String (Id_S);

      --  Test type.
      if Test_Type_S = "local" then
         Result.Test_Type := Local;
      else
         raise Program_Error;
      end if;

      --  Test timeout.
      declare
         Timeout_S : constant String := Get_Conf (Section, "timeout");
      begin
         Result.Timeout := Integer'Value (Timeout_S);
         pragma Debug (O ("timeout is " & Integer'Image (Result.Timeout)));
      exception
         when others =>
            Result.Timeout := Default_Timeout;
      end;

      --  Test executable.
      Result.Executable := To_Unbounded_String (Executable_S);

      return Result;
   end Read_Test;

   -----------------
   -- Launch_Test --
   -----------------

   procedure Launch_Test (Test_To_Run : Test_Case)
   is
      use GNAT.Expect;

      Result   : Expect_Match;
      Fd       : Process_Descriptor;
      Null_Argument_List : Argument_List := (1 => new String'(""));
      Item_To_Match : constant Regexp_Array
        := Regexp_Array'(+"FAILED",
                         +"END TESTS(.*)PASSED");

   begin
      Put_Line ("Launching test: " & To_String (Test_To_Run.Id));

      Non_Blocking_Spawn (Fd,
                          "./" & To_String (Test_To_Run.Executable),
                          Null_Argument_List);
      Add_Filter (Fd, Trace_Filter'Access, Output);
      Expect (Fd, Result, Item_To_Match, Test_To_Run.Timeout);
      case Result is
         when 1 =>
            Put_Line ("==> Test failed <==");

         when 2 =>
            Put_Line ("==> Test finished <==");

         when Expect_Timeout =>
            Put_Line ("==> Time Out ! <==");

         when others =>
            null;

      end case;

      New_Line;
      Close (Fd);
   end Launch_Test;

   ------------------
   -- Run_Scenario --
   ------------------

   procedure Run_Scenario
   is
      Scenario_Name : constant String :=
        Get_Conf ("scenario", "name");

      Scenario_Id : constant String :=
        Get_Conf ("scenario", "id");

      Count : Integer := 0;
      Test : Test_Case;
   begin
      Put_Line ("Running scenario: " & Scenario_Name);
      Put_Line ("Description: " & Scenario_Id);
      New_Line;

      loop
         Test := Read_Test (Scenario_Name, Count);
         exit when Test = Null_Test;

         Launch_Test (Test);
         Count := Count + 1;
      end loop;

      Put_Line ("All tests done in scenario: " & Scenario_Name);
      New_Line;
   end Run_Scenario;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      New_Line;
      Put_Line ("Usage: test_driver -scenario scenario_file|full directory,");
      Put_Line ("  -scenario scenario_file : plays scenario_file,");
      Put_Line ("  -full                   : plays all scenarios" &
                "in directory.");
      New_Line;
   end Usage;

   --  Main procedure begins here.

begin
   O ("Test driver launched.");

   loop
      case Getopt ("scenario: full:") is
         when ASCII.NUL =>
            exit;

         when 's' =>
            if Full_Switch = "scenario" then
               O ("Loading scenario file: " & Parameter);
               Load_Configuration_File (Parameter);
               Run_Scenario;

            end if;

         when 'f' =>
            if Full_Switch = "full" then
               O ("Running all scenario from: " & Parameter);
               declare
                  Dir      : Dir_Type;
                  Scenario : String (1 .. 1024);
                  Last     : Natural;
               begin
                  Open (Dir, Parameter);
                  loop
                     Read (Dir, Scenario, Last);
                     exit when Scenario (1 .. Last) = "";

                     declare
                        Full_Name : constant String :=
                          Parameter & "/" & Scenario (1 .. Last);
                     begin
                        if Is_Regular_File (Full_Name) then
                           Load_Configuration_File (Full_Name);
                           Run_Scenario;
                        end if;
                     end;
                  end loop;
               end;
            end if;
         when others =>
            raise Program_Error;
      end case;
   end loop;

exception
   when Invalid_Switch =>
      Put_Line ("Invalid Switch " & Full_Switch);
      Usage;
   when Invalid_Parameter =>
      Put_Line ("No parameter for " & Full_Switch);
      Usage;
end Test_Driver;
