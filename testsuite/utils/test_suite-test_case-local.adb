------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--           T E S T _ S U I T E . T E S T _ C A S E . L O C A L            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
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

with GNAT.Expect;
with Test_Suite.Run;

package body Test_Suite.Test_Case.Local is

   use GNAT.Expect;
   use Test_Suite.Run;

   --------------
   -- Run_Test --
   --------------

   function Run_Test
     (Test_To_Run : Local_Test;
      Output      : Test_Suite_Output'Class)
     return Boolean
   is
      Test_Result : Boolean;

   begin
      Log (Output, "Launching test: " & To_String (Test_To_Run.Id));
      Separator (Output);

      Test_Result
        := Run.Run
           (Output,
            Test_To_Run.Exec,
            Test_To_Run.Exec_In_Base_Directory,
            "",
            Regexp_Array'(+"END TESTS(.*)FAILED",
                          +"END TESTS(.*)PASSED"),
            Analyze_CB_Array'(Parse_Failure'Access,
                              Parse_Success'Access),
            Test_To_Run.Timeout);

      Close_Test_Output_Context
        (Output,
         Test_Result xor Test_To_Run.Expected_Failure);

      return Test_Result;
   end Run_Test;

end Test_Suite.Test_Case.Local;
