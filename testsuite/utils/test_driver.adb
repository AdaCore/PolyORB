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

--  Wrapper to launch PolyORB's testsuite.

--  $Id$

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Regpat;

with PolyORB.Configuration;
with PolyORB.Log;

procedure Test_Driver is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use GNAT.Command_Line;
   use GNAT.Directory_Operations;
   use GNAT.OS_Lib;
   use GNAT.Regpat;

   use PolyORB.Configuration;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("test_driver");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   type Test_Kind is
     (Local,
      Client_Server);

   type Executable is record
      Command  : Unbounded_String;
      --  Command to run.

      Conf     : Unbounded_String;
      --  Associated configuration file if required.
   end record;

   Null_Executable : constant Executable :=
     (Command => To_Unbounded_String (""),
      Conf    => To_Unbounded_String (""));

   type Exec_List is array (Positive range <>) of Executable;

   type Test_Case is record
      Id         : Ada.Strings.Unbounded.Unbounded_String;
      Test_Type  : Test_Kind := Local;
      Timeout    : Integer := 0;
      Exe_To_Run : Exec_List (1 .. 2);
   end record;

   Null_Test : constant Test_Case :=
     (Id         => To_Unbounded_String (""),
      Test_Type  => Local,
      Timeout    => 0,
      Exe_To_Run => (others => Null_Executable));

   function Extract_Test
     (Scenario : String;
      Number   : Natural)
     return Test_Case;
   --  Extract test case #Number from scenario file.
   --  XXX Ugly, need to define an adapted configuration file.

   procedure Launch_Test (Test_To_Run : Test_Case);
   --  Launch test.

   procedure Run_Scenario (Scenario_File : String);
   --  Run scenario file.

   procedure Run_All_Scenarios (Directory_Name : String);
   --  Run all scenarios from 'Directory_Name' directory.

   procedure Usage;
   --  Print usage information.

   ------------------
   -- Extract_Test --
   ------------------

   function Extract_Test
     (Scenario : String;
      Number   : Natural)
     return Test_Case
   is
      Default_Timeout : constant Integer := 10_000;

      Test_Id : constant String := Natural'Image (Number);
      Section : constant String
        := "test " & Scenario & "_"
        & Test_Id (Test_Id'First + 1 .. Test_Id'Last);

      Result : Test_Case;

      Id_S         : constant String := Get_Conf (Section, "id");
      Test_Type_S  : constant String := Get_Conf (Section, "type");
   begin
      --  Is there a test to extract ?
      if Id_S = "" and then Test_Type_S = "" then
         return Null_Test;
      end if;

      O ("Read     : " & Section);
      O (" Id      : " & Id_S);
      O (" Type    : " & Test_Type_S);

      --  Test Id.
      Result.Id := To_Unbounded_String (Id_S);

      --  Test type.
      if Test_Type_S = "local" then
         Result.Test_Type := Local;
      elsif Test_Type_S = "client_server" then
         Result.Test_Type := Client_Server;
      else
         Put_Line ("Syntax error in scenario file.");
         raise Program_Error;
      end if;

      --  Test timeout.
      declare
         Timeout_S : constant String := Get_Conf (Section, "timeout");
      begin
         Result.Timeout := Integer'Value (Timeout_S);
         O (" Timeout :" & Integer'Image (Result.Timeout));
      exception
         when others =>
            Result.Timeout := Default_Timeout;
      end;

      --  Test executable(s) to run.
      case Result.Test_Type is
         when Local =>
            declare
               Command_S : constant String := Get_Conf (Section, "command");
            begin
               Result.Exe_To_Run (1).Command
                 := To_Unbounded_String (Command_S);
            end;

         when Client_Server =>
            declare
               Client_Section : constant String
                 := "client " & Scenario & "_"
                 & Test_Id (Test_Id'First + 1 .. Test_Id'Last);

               Client_S : constant String :=
                 Get_Conf (Client_Section, "command");

               Server_Section : constant String
                 := "server " & Scenario & "_"
                 & Test_Id (Test_Id'First + 1 .. Test_Id'Last);

               Server_S : constant String :=
                 Get_Conf (Server_Section, "command");
            begin
               Result.Exe_To_Run (1).Command
                 := To_Unbounded_String (Server_S);

               Result.Exe_To_Run (2).Command
                 := To_Unbounded_String (Client_S);

            end;
      end case;
      return Result;
   end Extract_Test;

   -----------------
   -- Launch_Test --
   -----------------

   procedure Launch_Test (Test_To_Run : Test_Case)
   is
      use GNAT.Expect;

      Null_Argument_List : constant Argument_List := (1 => new String'(""));

      Item_To_Match : constant Regexp_Array
        := Regexp_Array'(+"FAILED",
                         +"END TESTS(.*)PASSED");

      -----------------------
      -- Launch_Local_Test --
      -----------------------

      procedure Launch_Local_Test (Test_To_Run : Test_Case);

      procedure Launch_Local_Test (Test_To_Run : Test_Case)
      is
         Result   : Expect_Match;
         Fd       : Process_Descriptor;
         Command  : constant String
           := "./" & To_String (Test_To_Run.Exe_To_Run (1).Command);

      begin
         --  Launch Test.
         Put_Line ("Running: " & Command);
         Non_Blocking_Spawn (Fd, Command, Null_Argument_List);

         --  Redirect Output.
         Add_Filter (Fd, Trace_Filter'Access, Output);

         --  Parse output.
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
      end Launch_Local_Test;

      -------------------------------
      -- Launch_Client_Server_Test --
      -------------------------------

      procedure Launch_Client_Server_Test (Test_To_Run : Test_Case);

      procedure Launch_Client_Server_Test (Test_To_Run : Test_Case)
      is
         Result    : Expect_Match;
         Fd_Server : Process_Descriptor;
         Fd_Client : Process_Descriptor;

         Match : Match_Array (0 .. 2);
         Server_Command : constant String
           := "./" & To_String (Test_To_Run.Exe_To_Run (1).Command);

         Client_Command : constant String
           := "./" & To_String (Test_To_Run.Exe_To_Run (2).Command);

         IOR_String : Unbounded_String;

      begin
         --  Launch Server.
         Put_Line ("Running server: " & Server_Command);
         Non_Blocking_Spawn (Fd_Server, Server_Command, Null_Argument_List);

         --  Match Server IOR.
         Add_Filter (Fd_Server, Trace_Filter'Access, Output);
         Expect (Fd_Server, Result, "IOR:(.*)", Match, -1);
         case Result is
            when 1 =>
               IOR_String := To_Unbounded_String
                 (Expect_Out (Fd_Server)
                  (Match (0).First .. Match (0).Last - 1));

            when others =>
               raise Program_Error;

         end case;

         --  Launch Client.
         New_Line;
         Put_Line ("Running client: " & Client_Command);
         declare
            Client_Argument_List : constant Argument_List
              := (1 => new String'(To_String (IOR_String)));
         begin
            Non_Blocking_Spawn (Fd_Client,
                                Client_Command,
                                Client_Argument_List);

            --  Redirect Output.
            Add_Filter (Fd_Client, Trace_Filter'Access, Output);

            --  Parse output.
            Expect (Fd_Client, Result, Item_To_Match, Test_To_Run.Timeout);
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
            Close (Fd_Client);
         end;

         New_Line;
         Close (Fd_Server);
      exception
         when others =>
            Close (Fd_Server);
      end Launch_Client_Server_Test;

   begin
      Put_Line ("Launching test: " & To_String (Test_To_Run.Id));

      if Test_To_Run.Test_Type = Local then
         Launch_Local_Test (Test_To_Run);

      elsif Test_To_Run.Test_Type = Client_Server then
         Launch_Client_Server_Test (Test_To_Run);
      end if;

   exception
      when GNAT.Expect.Process_Died =>
         --  The process may normally exit or die because of an internal
         --  error. We cannot judge at this stage.

         Put_Line ("==> Process Terminated <==");
         New_Line;

      when others =>
         raise;

   end Launch_Test;

   ------------------
   -- Run_Scenario --
   ------------------

   procedure Run_Scenario (Scenario_File : String) is

      ---------------------
      -- Launch_Scenario --
      ---------------------

      procedure Launch_Scenario;
      --  Execute all tests in scenario file.

      procedure Launch_Scenario
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
            Test := Extract_Test (Scenario_Name, Count);
            exit when Test = Null_Test;

            Launch_Test (Test);
            Count := Count + 1;
         end loop;

         Put_Line ("All tests done in scenario: " & Scenario_Name);
         New_Line;
      end Launch_Scenario;

   begin
      Load_Configuration_File (Scenario_File);
      Launch_Scenario;
   end Run_Scenario;

   -----------------------
   -- Run_All_Scenarios --
   -----------------------

   procedure Run_All_Scenarios (Directory_Name : String)
   is
      Dir      : Dir_Type;
      Scenario : String (1 .. 1024);
      Last     : Natural;

   begin
      Put_Line ("Running all scenario from: " & Directory_Name);

      Open (Dir, Directory_Name);
      loop
         Read (Dir, Scenario, Last);
         exit when Scenario (1 .. Last) = "";

         declare
            Full_Name : constant String :=
              Directory_Name & "/" & Scenario (1 .. Last);
         begin
            if Is_Regular_File (Full_Name) then
               Run_Scenario (Full_Name);
            end if;
         end;
      end loop;
   end Run_All_Scenarios;

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
   Put_Line ("Test driver launched.");

   loop
      case Getopt ("scenario: full:") is
         when ASCII.NUL =>
            exit;

         when 's' =>
            if Full_Switch = "scenario" then
               Run_Scenario (Parameter);
            end if;

         when 'f' =>
            if Full_Switch = "full" then
               Run_All_Scenarios (Parameter);
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

   when E : others =>
      Put_Line ("==> Internal Error <==");
      Put_Line (" Got exception: "
                & Ada.Exceptions.Exception_Name (E)
                & ", "
                & Ada.Exceptions.Exception_Message (E));

end Test_Driver;
