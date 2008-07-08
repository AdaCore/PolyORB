------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                    T E S T _ S U I T E . O U T P U T                     --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides support for different outputs during a test
--  suite run.

with GNAT.Expect;
with System;

package Test_Suite.Output is

   type Test_Suite_Output is abstract tagged private;

   type TSO_Access is access all Test_Suite_Output'Class;

   procedure Open (Output : Test_Suite_Output) is abstract;
   --  Initialize Output

   procedure Close (Output : Test_Suite_Output) is abstract;
   --  Finalize Output

   procedure Error
     (Output    : Test_Suite_Output;
      Error_Msg : String)
      is abstract;
   --  Output an error message

   procedure Log
     (Output : Test_Suite_Output;
      Log_Msg : String)
      is abstract;
   --  Output a log message

   procedure Test_Execution
     (Output : Test_Suite_Output;
      Msg    : String)
      is abstract;
   --  Output test execution output

   procedure Separator (Output : Test_Suite_Output) is abstract;
   --  Output a Separator

   procedure Open_Test_Output_Context
     (Output : Test_Suite_Output;
      Name   : String)
      is abstract;
   --  Open a new Context in which Test execution messages will be output

   procedure Close_Test_Output_Context
     (Output : Test_Suite_Output;
      Result : Boolean)
      is abstract;
   --  Close output Context

   procedure Open_Scenario_Output_Context
     (Output : Test_Suite_Output;
      Name   : String)
      is abstract;
   --  Open a new higher level Context in which Log messages will be output

   procedure Close_Scenario_Output_Context
     (Output : Test_Suite_Output;
      Result : Boolean)
      is abstract;
   --  Close output Context

   procedure Initialize_Filter (Output : Test_Suite_Output'Class);
   --  Initialize Output Filter

   procedure Output_Filter
     (Descriptor : GNAT.Expect.Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Output Filter used when running a file, see GNAT.Expect for more
   --  details.

private

   type Test_Suite_Output is abstract tagged null record;

end Test_Suite.Output;
