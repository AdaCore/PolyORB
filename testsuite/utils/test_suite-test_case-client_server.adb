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
      function Run_Client (IOR : String) return Boolean;
      function Run_Server return Boolean;

      ----------------
      -- Run_Client --
      ----------------

      function Run_Client (IOR : String) return Boolean is
         Fd : Process_Descriptor;

         Command : constant String
           := "./" & To_String (Test_To_Run.Client.Command);

         Env : constant String := To_String (Test_To_Run.Client.Conf);

         Argument_List : GNAT.OS_Lib.Argument_List := (1 => new String'(IOR));

         Result : Expect_Match;

         Item_To_Match : constant Regexp_Array
           := Regexp_Array'(+"END TESTS(.*)FAILED",
                            +"END TESTS(.*)PASSED");

         Test_Result : Boolean;

      begin
         --  Setting environment

         if Env = "" then
            Log (Output, "No environment to set.");
            Setenv ("POLYORB_CONF", Env);
         else
            Log (Output, "Setting environment: " & Env);
            Setenv ("POLYORB_CONF", Env);
         end if;

         --  Test the executable actually exists

         if not Is_Regular_File (Command) then
            Log (Output, Command & " does not exist !");
            Log (Output, "Aborting test");

            return False;
         end if;

         --  Launch Test

         Log (Output, "Running client: " & Command);
         Log (Output, "  with timeout: "
              & Integer'Image (Test_To_Run.Timeout));
         Log (Output, "           IOR:");
         Log (Output, "'" & IOR & "'");
         Separator (Output);

         --  Spawn Client

         Non_Blocking_Spawn
           (Descriptor  => Fd,
            Command     => Command,
            Args        => Argument_List,
            Buffer_Size => 4096,
            Err_To_Out  => True);

         --  Redirect Output

         Initialize_Filter (Output);
         Add_Filter (Fd, Output_Filter'Access, GNAT.Expect.Output);

         --  Parse output

         Expect
           (Fd,
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

         Free (Argument_List (1));
         Close (Fd);

         return Test_Result;

      exception
         when GNAT.Expect.Process_Died =>

            Log (Output, "==> Client Process Terminated <==");

            --  The process may normally exit, or die because of an
            --  internal error. We cannot judge at this stage.

            Free (Argument_List (1));
            Close (Fd);

            return False;

         when others =>
            Free (Argument_List (1));
            Close (Fd);

            raise;
      end Run_Client;

      ----------------
      -- Run_Server --
      ----------------

      function Run_Server return Boolean is
         Fd : Process_Descriptor;

         Command : constant String
           := "./" & To_String (Test_To_Run.Server.Command);

         Env    : constant String := To_String (Test_To_Run.Server.Conf);

         Argument_List : GNAT.OS_Lib.Argument_List := (1 => new String'(""));

         Result : Expect_Match;

         Match  : Match_Array (0 .. 0);

         Item_To_Match : constant Regexp_Array
           := Regexp_Array'(1 => +"IOR:([a-z0-9]*)");

         Test_Result : Boolean;

      begin
         --  Setting environment

         if Env = "" then
            Log (Output, "No environment to set.");
            Setenv ("POLYORB_CONF", Env);
         else
            Log (Output, "Setting environment: " & Env);
            Setenv ("POLYORB_CONF", Env);
         end if;

         --  Test the executable actually exists

         if not Is_Regular_File (Command) then
            Log (Output, Command & " does not exist !");
            Log (Output, "Aborting test");

            return False;
         end if;

         --  Launch Test

         Log (Output, "Running server: " & Command);
         Separator (Output);

         --  Spawn Server

         Non_Blocking_Spawn
           (Descriptor  => Fd,
            Command     => Command,
            Args        => Argument_List,
            Buffer_Size => 4096,
            Err_To_Out  => True);

         --  Redirect Output

         Initialize_Filter (Output);
         Add_Filter (Fd, Output_Filter'Access, GNAT.Expect.Output);

         --  Match Server IOR

         Expect (Fd, Result, Item_To_Match, Match, Test_To_Run.Timeout);

         --  Parse output

         case Result is
            when 1 =>
               --  Start Client

               Test_Result := Run_Client
                 (Expect_Out (Fd)
                  (Match (0).First .. Match (0).Last));

            when others =>
               Log (Output, "Error when parsing server IOR");
               Test_Result := False;
         end case;

         --  Clean up

         Free (Argument_List (1));
         Close (Fd);

         return Test_Result;

      exception
         when GNAT.Expect.Process_Died =>

            --  The process may normally exit, or die because of an
            --  internal error. We cannot judge at this stage.

            Log (Output, "==> Server Process Terminated <==");
            Test_Result := False;

            Free (Argument_List (1));
            Close (Fd);

            return Test_Result;

         when others =>
            Free (Argument_List (1));
            Close (Fd);

            raise;
      end Run_Server;

      Test_Result : Boolean;

   begin
      Log (Output, "Launching test: " & To_String (Test_To_Run.Id));
      Separator (Output);

      Test_Result := Run_Server;
      Close_Test_Output_Context (Output, Test_Result);

      return Test_Result;
   end Run_Test;

end Test_Suite.Test_Case.Client_Server;
