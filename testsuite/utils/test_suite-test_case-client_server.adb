------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   T E S T _ S U I T E . T E S T _ C A S E . C L I E N T _ S E R V E R    --
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

--  $Id$

with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.Regpat;

package body Test_Suite.Test_Case.Client_Server is

   use GNAT.Expect;
   use GNAT.OS_Lib;
   use GNAT.Regpat;

   Null_Argument_List : constant Argument_List := (1 => new String'(""));

   Item_To_Match : constant Regexp_Array
     := Regexp_Array'(+"FAILED",
                      +"END TESTS(.*)PASSED");

   --------------
   -- Run_Test --
   --------------

   function Run_Test
     (Test_To_Run : Client_Server_Test;
      Output      : Test_Suite_Output'Class)
     return Boolean
   is
      Result : Expect_Match;

      Fd_Server : Process_Descriptor;
      Fd_Client : Process_Descriptor;

      IOR_String : Unbounded_String;

      Test_Result : Boolean;

   begin
      Log (Output, "Launching test: " & To_String (Test_To_Run.Id));
      Separator (Output);

      --  Launch Server.

      declare
         Server_Command : constant String
           := "./" & To_String (Test_To_Run.Server.Command);

         Match  : Match_Array (0 .. 2);
         Env    : constant String := To_String (Test_To_Run.Server.Conf);
      begin
         --  Setting environment.
         if Env = "" then
            Log (Output, "No environment to set.");
         else
            Log (Output, "Setting environment: " & Env);
            Setenv ("POLYORB_CONF", Env);
         end if;

         --  Spawn Server.
         Log (Output, "Running server: " & Server_Command);
         Separator (Output);

         Non_Blocking_Spawn (Fd_Server, Server_Command, Null_Argument_List);

         --  Match Server IOR.
         Initialize_Filter (Output);
         Add_Filter (Fd_Server, Output_Filter'Access, GNAT.Expect.Output);
         Expect (Fd_Server, Result, "IOR:([a-z0-9]*)", Match, -1);

         case Result is
            when 1 =>
               IOR_String := To_Unbounded_String
                 (Expect_Out (Fd_Server)
                  (Match (0).First .. Match (0).Last));

            when others =>
               Log (Output, "Error when parsing server IOR");
               raise Program_Error;

         end case;
      end;

      Separator (Output);

      --  Launch Client.

      declare
         Client_Argument_List : constant Argument_List
           := (1 => new String'(To_String (IOR_String)));

         Client_Command : constant String
           := "./" & To_String (Test_To_Run.Client.Command);

         Env    : constant String := To_String (Test_To_Run.Client.Conf);
      begin
         --  Setting environment.
         if Env = "" then
            Log (Output, "No environment to set.");
         else
            Log (Output, "Setting environment: " & Env);
            Setenv ("POLYORB_CONF", Env);
         end if;

         --  Spawn Client.
         Log (Output, "Running client: " & Client_Command);
         Separator (Output);

         Non_Blocking_Spawn (Fd_Client,
                             Client_Command,
                             Client_Argument_List);

         --  Redirect Output.
         Initialize_Filter (Output);
         Add_Filter (Fd_Client, Output_Filter'Access, GNAT.Expect.Output);

         --  Parse output.
         Expect (Fd_Client, Result, Item_To_Match, Test_To_Run.Timeout);
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

         Close (Fd_Client);
      end;

      Close (Fd_Server);
      Close_Test_Output_Context (Output, Test_Result);

      return Test_Result;

   exception
      when GNAT.Expect.Process_Died =>
         --  The process may normally exit, or die because of an internal
         --  error. We cannot judge at this stage.

         Log (Output, "==> Process Terminated <==");
         Test_Result := True;

         Close (Fd_Server);
         Close_Test_Output_Context (Output, Test_Result);

         return Test_Result;

      when others =>
         Close (Fd_Server);
         raise;

   end Run_Test;

end Test_Suite.Test_Case.Client_Server;
