------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E S T _ S U I T E . O U T P U T . T E X T                --
--                                                                          --
--                                 S p e c                                  --
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

package Test_Suite.Output.Text is

   type Text_Output is new Test_Suite_Output with private;

   procedure Open (Output : Text_Output);
   --  Initialize Output.

   procedure Close (Output : Text_Output);
   --  Finalize Output.

   procedure Error
     (Output    : Text_Output;
      Error_Msg : String);
   --  Output an error message.

   procedure Log
     (Output : Text_Output;
      Log_Msg : String);
   --  Output a log message.

   procedure Test_Execution
     (Output : Text_Output;
      Msg    : String);
   --  Output test execution output.

   procedure Separator (Output : Text_Output);
   --  Output a 'Separator'.

   procedure Open_Test_Output_Context
     (Output : Text_Output;
      Name   : String);
   --  Open a new Context in which Test execution messages will be output.

   procedure Close_Test_Output_Context
     (Output : Text_Output;
      Result : Boolean);

   procedure Open_Scenario_Output_Context
     (Output : Text_Output;
      Name   : String);

   procedure Close_Scenario_Output_Context
     (Output : Text_Output;
      Result : Boolean);

private

   type Text_Output is new Test_Suite_Output with null record;

end Test_Suite.Output.Text;
