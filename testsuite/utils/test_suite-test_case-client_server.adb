------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   T E S T _ S U I T E . T E S T _ C A S E . C L I E N T _ S E R V E R    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2004 Free Software Foundation, Inc.           --
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

with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Regpat;

package body Test_Suite.Test_Case.Client_Server is

   use GNAT.Expect;
   use GNAT.OS_Lib;
   use GNAT.Regpat;

   --------------
   -- Run_Test --
   --------------

   function Run_Test
     (Test_To_Run : Client_Server_Test;
      Output      : Test_Suite_Output'Class)
     return Boolean
   is
      Test_Result : Boolean := True;

      procedure Run_Client (IOR : String);
      procedure Run_Server;

      ----------------
      -- Run_Client --
      ----------------

      procedure Run_Client (IOR : String) is
         Fd_Client : Process_Descriptor;

         Client_Command : constant String
           := "./" & To_String (Test_To_Run.Client.Command);

         Env    : constant String := To_String (Test_To_Run.Client.Conf);

         Result : Expect_Match;

         Client_Argument_List : Argument_List
           := (1 => new String'(IOR));

         Item_To_Match : constant Regexp_Array
           := Regexp_Array'(+"END TESTS(.*)FAILED",
                            +"END TESTS(.*)PASSED");
      begin
         --  Setting environment

         if Env = "" then
            Log (Output, "No environment to set.");
            Setenv ("POLYORB_CONF", Env);
         else
            Log (Output, "Setting environment: " & Env);
            Setenv ("POLYORB_CONF", Env);
         end if;

         Log (Output, "Running client: " & Client_Command);
         Log (Output, "  with timeout: "
              & Integer'Image (Test_To_Run.Timeout));
         Log (Output, "           IOR:");
         Log (Output, "'" & IOR & "'");
         Separator (Output);

         --  Test the executable actually exists

         if not Is_Regular_File (Client_Command) then
            Log (Output, Client_Command & " does not exist !");
            Log (Output, "Aborting test");

            Test_Result := False;

            return;
         end if;

         --  Spawn Client

         Non_Blocking_Spawn
           (Descriptor  => Fd_Client,
            Command     => Client_Command,
            Args        => Client_Argument_List,
            Buffer_Size => 4096,
            Err_To_Out  => True);

         --  Redirect Output

         Initialize_Filter (Output);
         Add_Filter (Fd_Client,
                     Output_Filter'Access,
                     GNAT.Expect.Output);

         --  Parse output

         Expect
           (Fd_Client,
            Result,
            Item_To_Match,
            Test_To_Run.Timeout);

         case Result is
            when 1 =>
               Log (Output, "==> Test failed <==");
               Test_Result := False;

            when 2 =>
               Log (Output, "==> Test finished <==");
               Test_Result := True;

            when Expect_Timeout =>
               Log (Output, "==> Time Out ! <==");
               Test_Result := False;

            when others =>
               Log (Output, "==> Unexpected output ! <==");
               Test_Result := False;
         end case;

         --  Clean up

         Free (Client_Argument_List (1));
         Close (Fd_Client);

      exception
         when GNAT.Expect.Process_Died =>

            Log (Output, "==> Client Process Terminated <==");

            --  The process may normally exit, or die because of an
            --  internal error. We cannot judge at this stage.

            Test_Result := False;

            Close (Fd_Client);

         when others =>
            Close (Fd_Client);
            raise;
      end Run_Client;

      ----------------
      -- Run_Server --
      ----------------

      procedure Run_Server is
         Fd_Server : Process_Descriptor;

         Server_Command : constant String
           := "./" & To_String (Test_To_Run.Server.Command);

         Match  : Match_Array (0 .. 2);
         Env    : constant String := To_String (Test_To_Run.Server.Conf);

         Server_Argument_List : Argument_List
           := (1 => new String'(""));

         Result : Expect_Match;

      begin
         --  Setting environment

         if Env = "" then
            Log (Output, "No environment to set.");
            Setenv ("POLYORB_CONF", Env);
         else
            Log (Output, "Setting environment: " & Env);
            Setenv ("POLYORB_CONF", Env);
         end if;

         Log (Output, "Running server: " & Server_Command);
         Separator (Output);

         --  Test the executable actually exists

         if not Is_Regular_File (Server_Command) then
            Log (Output, Server_Command & " does not exist !");
            Log (Output, "Aborting test");

            Test_Result := False;

            return;
         end if;

         --  Spawn Server

         Non_Blocking_Spawn
           (Descriptor  => Fd_Server,
            Command     => Server_Command,
            Args        => Server_Argument_List,
            Buffer_Size => 4096,
            Err_To_Out  => True);

         Initialize_Filter (Output);
         Add_Filter
           (Fd_Server,
            Output_Filter'Access,
            GNAT.Expect.Output);

         --  Match Server IOR

         Expect (Fd_Server, Result, "IOR:([a-z0-9]*)", Match, -1);

         --  Parse output

         case Result is
            when 1 =>
               --  Start Client Task

               --               Client_T.Initialize
               Run_Client
                 (Expect_Out (Fd_Server)
                  (Match (0).First .. Match (0).Last));

            when others =>
               Log (Output, "Error when parsing server IOR");

         end case;

         --  Clean up

         Free (Server_Argument_List (1));
         Close (Fd_Server);

      exception
         when GNAT.Expect.Process_Died =>

            --  The process may normally exit, or die because of an
            --  internal error. We cannot judge at this stage.

            Log (Output, "==> Server Process Terminated <==");
            Test_Result := False;

            Close (Fd_Server);

         when others =>
            Close (Fd_Server);
            raise;
      end Run_Server;

   begin
      Log (Output, "Launching test: " & To_String (Test_To_Run.Id));
      Separator (Output);

      Run_Server;
      Close_Test_Output_Context (Output, Test_Result);

      return Test_Result;
   end Run_Test;

end Test_Suite.Test_Case.Client_Server;
