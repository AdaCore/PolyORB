------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T _ S U I T E . T E S T _ C A S E                  --
--                                                                          --
--                                 S p e c                                  --
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

--  This package provides support for different test types and their
--  associated execution process.

with Ada.Strings.Unbounded;
with GNAT.OS_Lib;

with Test_Suite.Output;

package Test_Suite.Test_Case is

   use Test_Suite.Output;

   use Ada.Strings.Unbounded;

   type Test is abstract tagged record
      Id                     : Unbounded_String;
      Timeout                : Integer := 0;
      Exec_In_Base_Directory : Boolean := False;
      Expected_Failure       : Boolean := False;
   end record;
   --  Base type for all test

   function Run_Test
     (Test_To_Run : Test;
      Output      : Test_Suite_Output'Class)
      return Boolean is abstract;
   --  Test process associated to a test

   type Null_Test is new Test with private;

   function Run_Test
     (Test_To_Run : Null_Test;
      Output      : Test_Suite_Output'Class)
     return Boolean;
   --  Null_Test does nothing; raises 'Program_Error' if run

   type Executable is record
      Command  : Unbounded_String;
      --  Command to run

      Conf     : Unbounded_String;
      --  Associated PolyORB's configuration file, if required

      Args : GNAT.OS_Lib.Argument_List_Access;

   end record;

   function Create
     (Command : Unbounded_String;
      Conf    : Unbounded_String;
      Args    : GNAT.OS_Lib.Argument_List_Access)
     return Executable;

private

   type Null_Test is new Test with null record;

end Test_Suite.Test_Case;
