------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . P A R A M E T E R S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2004 Free Software Foundation, Inc.           --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  PolyORB runtime configuration facility.

package PolyORB.Parameters is

   pragma Elaborate_Body;

   procedure Set_Hooks;
   --  Set Get_Conf hooks in units that need to access parameters
   --  but cannot depend on this package.

   procedure Set_Conf
     (Section, Key : String;
      Value        : String);
   --  Sets the value of the given Key in the named Section.

   function Get_Conf
     (Section, Key : String;
      Default : String := "")
     return String;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined.

   function Get_Conf
     (Section, Key : String;
      Default : Boolean := False)
     return Boolean;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as a Boolean:
   --  * True if the value starts with '1' or 'Y' or 'y',
   --    or is "on" or "enable" or "true"
   --  * False if the value starts with '0' or 'n' or 'N',
   --    or is "off" or "disable" or "false" or empty.
   --  Constraint_Error is raised if the value is set to anything else.

   function Get_Conf
     (Section, Key : String;
      Default : Integer := 0)
     return Integer;
   --  Return the value of the global variable Key or Default if this
   --  variable is not defined, interpreting the value as the decimal
   --  representation of an integer number.
   --  Constraint_Error is raised if the value is set to anything else.

   type Parameters_Initializer is access procedure;

   procedure Reset;
   --  Clear all variables previously positioned using Set_Conf.

private

   function Get_Env
     (Key : String;
      Default : String := "")
     return String;
   --  Get the value of variable Key from the system
   --  environment variables, returning Default if not found.

   type Fetch_From_File_T is access function (Key : String) return String;

   Fetch_From_File_Hook : Fetch_From_File_T := null;

end PolyORB.Parameters;
