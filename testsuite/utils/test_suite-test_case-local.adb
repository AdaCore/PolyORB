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

   Null_Argument_List : constant Argument_List := (1 => new String'(""));

   Item_To_Match : constant Regexp_Array
     := Regexp_Array'(+"FAILED",
                      +"END TESTS(.*)PASSED");

   --------------
   -- Run_Test --
   --------------

   function Run_Test
     (Test_To_Run : Local_Test;
      Output      : Test_Suite_Output'Class)
     return Boolean
   is
      Result   : Expect_Match;
      Fd       : Process_Descriptor;
      Command  : constant String
        := "./" & To_String (Test_To_Run.Exec.Command);

      Test_Result : Boolean;
   begin
      Log (Output, "Launching test: " & To_String (Test_To_Run.Id));
      Separator (Output);

      --  Launch Test

      Log (Output, "Running: " & Command);
      Separator (Output);

      --  Spawn Executable

      Non_Blocking_Spawn
        (Descriptor  => Fd,
         Command     => Command,
         Args        => Null_Argument_List,
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

      Close (Fd);
      Close_Test_Output_Context (Output, Test_Result);

      return Test_Result;

   exception

      when GNAT.Expect.Process_Died =>

         --  If we catch this exception before the test program
         --  produces expected output then the test failed.

         Log (Output, "==> Process terminated abnormally <==");
         Test_Result := False;

         Close (Fd);
         Close_Test_Output_Context (Output, Test_Result);

         return Test_Result;
   end Run_Test;

end Test_Suite.Test_Case.Local;
