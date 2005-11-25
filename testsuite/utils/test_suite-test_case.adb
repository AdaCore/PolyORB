------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 T E S T _ S U I T E . T E S T _ C A S E                  --
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

package body Test_Suite.Test_Case is

   ------------
   -- Create --
   ------------

   function Create
     (Command : Unbounded_String;
      Conf    : Unbounded_String;
      Args    : GNAT.OS_Lib.Argument_List_Access)
     return Executable
   is
   begin
      return Executable'(Command, Conf, Args);
   end Create;

   --------------
   -- Run_Test --
   --------------

   function Run_Test
     (Test_To_Run : Null_Test;
      Output      : Test_Suite_Output'CLass)
     return Boolean
   is
      pragma Warnings (Off); --  WAG:3.14
      pragma Unreferenced (Test_To_Run);
      pragma Unreferenced (Output);
      pragma Warnings (On); --  WAG:3.14

   begin
      raise Program_Error;
      --  Should not come to this point

      return False;
   end Run_Test;

end Test_Suite.Test_Case;
