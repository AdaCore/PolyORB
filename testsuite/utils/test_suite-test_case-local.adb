------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           T E S T _ S U I T E . T E S T _ C A S E . L O C A L            --
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

--  $Id$

with GNAT.Expect;
with GNAT.OS_Lib;

package body Test_Suite.Test_Case.Local is

   use GNAT.Expect;
   use GNAT.OS_Lib;

   --------------
   -- Run_Test --
   --------------

   function Run_Test
     (Test_To_Run : Local_Test;
      Output      : Test_Suite_Output'Class)
     return Boolean
   is

      function Run_Local return Boolean;

      ---------------
      -- Run_Local --
      ---------------

      function Run_Local return Boolean is
         Fd       : Process_Descriptor;

         Command  : constant String
           := "./" & To_String (Test_To_Run.Exec.Command);

         Env : constant String := To_String (Test_To_Run.Exec.Conf);

         Argument_List : GNAT.OS_Lib.Argument_List := (1 => new String'(""));

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

            Test_Result := False;

            Close_Test_Output_Context (Output, Test_Result);

            return Test_Result;
         end if;

         --  Launch Test

         Log (Output, "Running: " & Command);
         Separator (Output);

         --  Spawn Executable

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

         Expect (Fd, Result, Item_To_Match, Test_To_Run.Timeout);

         case Result is
            when 1 =>
               Log (Output, "==> Test failed <==");
               Test_Result := False;

            when 2 =>
               Log (Output, "==> Test finished <==");
               Test_Result := True;

            when Expect_Timeout =>
               Log (Output, "==> Time out ! <==");
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

            --  If we catch this exception before the test program
            --  produces expected output then the test failed.

            Log (Output, "==> Process terminated abnormally <==");
            Test_Result := False;

            Free (Argument_List (1));
            Close (Fd);

            return Test_Result;

         when others =>
            Free (Argument_List (1));
            Close (Fd);

            raise;
      end Run_Local;

      Test_Result : Boolean;

   begin
      Log (Output, "Launching test: " & To_String (Test_To_Run.Id));
      Separator (Output);

      Test_Result := Run_Local;
      Close_Test_Output_Context (Output, Test_Result);

      return Test_Result;
   end Run_Test;

end Test_Suite.Test_Case.Local;
