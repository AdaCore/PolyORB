------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          T E S T _ S U I T E . T E S T _ C A S E . P A R S E R           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Parameters;

with Test_Suite.Test_Case.Local;
with Test_Suite.Test_Case.Client_Server;

package body Test_Suite.Test_Case.Parser is

   use Ada.Strings.Unbounded;

   use PolyORB.Parameters;

   use Test_Suite.Test_Case.Local;
   use Test_Suite.Test_Case.Client_Server;

   ------------------
   -- Extract_Test --
   ------------------

   function Extract_Test
     (Scenario : String;
      Number   : Natural;
      Output   : Test_Suite_Output'Class)
     return Test'Class
   is
      Default_Timeout : constant Integer := 10_000;

      Test_Id : constant String := Natural'Image (Number);

      Test_Name : constant String :=
        Scenario & "_" & Test_Id (Test_Id'First + 1 .. Test_Id'Last);

      Section : constant String :=
        "test " & Test_Name;

      Id_S         : constant String := Get_Conf (Section, "id");
      Test_Type_S  : constant String := Get_Conf (Section, "type");
      Timeout      : Integer;

   begin
      --  Is there a test to extract ?
      if Id_S = "" and then Test_Type_S = "" then
         declare
            Result : Null_Test;
         begin
            return Result;
         end;
      end if;

      Open_Test_Output_Context (Output, Test_Name);
      Log (Output, "Read     : " & Section);
      Log (Output, " Id      : " & Id_S);
      Log (Output, " Type    : " & Test_Type_S);

      --  Test timeout.
      declare
         Timeout_S : constant String := Get_Conf (Section, "timeout");
      begin
         Timeout := Integer'Value (Timeout_S);
         Log (Output, " Timeout :" & Integer'Image (Timeout));
      exception
         when others =>
            Timeout := Default_Timeout;
            Log (Output, " Timeout : (default)" & Integer'Image (Timeout));
      end;

      Separator (Output);

      --  Test type.
      if Test_Type_S = "local" then
         declare
            Command_S : constant String := Get_Conf (Section, "command");
            Config_S : constant String := Get_Conf (Section, "config");

            Result : Local_Test;
         begin
            --  Test Id.
            Result.Id := To_Unbounded_String (Id_S);

            --  Test timeout.
            Result.Timeout := Timeout;

            --  Test executable.
            Result.Exec.Command := To_Unbounded_String (Command_S);
            Result.Exec.Conf := To_Unbounded_String (Config_S);

            return Result;
         end;

      elsif Test_Type_S = "client_server" then
         declare
            Client_Section : constant String
              := "client " & Scenario & "_"
              & Test_Id (Test_Id'First + 1 .. Test_Id'Last);

            Client_S : constant String :=
              Get_Conf (Client_Section, "command");

            Client_Config_S : constant String :=
              Get_Conf (Client_Section, "config_file");

            Server_Section : constant String
              := "server " & Scenario & "_"
              & Test_Id (Test_Id'First + 1 .. Test_Id'Last);

            Server_S : constant String :=
              Get_Conf (Server_Section, "command");

            Server_Config_S : constant String :=
              Get_Conf (Server_Section, "config_file");

            Result : Client_Server_Test;
         begin
            --  Test Id.
            Result.Id := To_Unbounded_String (Id_S);

            --  Test timeout.
            Result.Timeout := Timeout;

            --  Test executables.
            Result.Server.Command := To_Unbounded_String (Server_S);
            Result.Server.Conf := To_Unbounded_String (Server_Config_S);

            Result.Client.Command := To_Unbounded_String (Client_S);
            Result.Client.Conf := To_Unbounded_String (Client_Config_S);

            return Result;
         end;

      else
         Error (Output, "Syntax error in scenario file.");
         raise Program_Error;
      end if;

   end Extract_Test;


end Test_Suite.Test_Case.Parser;
